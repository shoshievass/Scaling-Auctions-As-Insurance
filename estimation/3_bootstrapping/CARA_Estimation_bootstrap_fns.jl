### Functions for running bootstrap estimation for the CARA model ###
### Note: This is set to use KNITRO for optimization ###
### Last edited June 3, 2022 ###
### svass@stanford.edu ###

# ENV["ARTELYS_LICENSE_NETWORK_ADDR"] = "license4.stanford.edu"

function getBootstrapContractDf(num_bootstraps, raw_contract_list)

    num_auctions = length(raw_contract_list);
    bs_list = Array{Any}(undef, num_bootstraps);
    for bs = 1:num_bootstraps
        sample_contract_indices = StatsBase.sample(Random.MersenneTwister(bs),1:num_auctions, num_auctions, replace=true)
        sample_contract_id_list = [raw_contract_list[i] for i in sample_contract_indices];
        bs_list[bs] = sample_contract_id_list
    end

    bs_df = DataFrame(bs = 1:num_bootstraps, bs_list = bs_list)

    return(bs_df)
end

function run_bootstrap_projGamma(sample_contract_list, sample_no, julia_data_dir, out_dir)
    @show julia_data_dir
    
    num_contracts = length(sample_contract_list)
    auction_arr = Vector{MinimalAuction}(undef, num_contracts)
    item_dict = Dict{Int64, Any}()
    item_cluster_dict = Dict{Int64, Any}()

    auc_arr_contract_nos = Vector{Int64}(undef, num_contracts)
    project_type_dict = Dict{Int64,Int64}() #dictionary mapping project type ids to the number of contracts of that type
    dollar_scale = 1000


    for i = 1:num_contracts
        contract = sample_contract_list[i]
        example_project = DataFrame(CSV.File(julia_data_dir*"sample_project_$contract.csv"))
        example_bidders = DataFrame(CSV.File(julia_data_dir*"sample_project_bidders_$contract.csv"))
        example_bidder_types = DataFrame(CSV.File(julia_data_dir*"sample_project_biddertypes_$contract.csv"))
        # example_aggbidder_types = DataFrame(CSV.File(julia_data_dir*"sample_project_aggbiddertypes_$contract.csv"))


        auction_arr[i] = constructMinimalAuction(example_project, example_bidders, example_bidder_types, dollar_scale)
        auc_arr_contract_nos[i] = auction_arr[i].contract_no
        auc_project_type_id = auction_arr[i].project_type_id
        
        try
            project_type_dict[auc_project_type_id] = project_type_dict[auc_project_type_id] + 1
        catch error
           if isa(error, KeyError)
            # project type id not found
            project_type_dict[auc_project_type_id] = 1
           end
        end

        for t in 1:auction_arr[i].T
            item_id = auction_arr[i].item_ids[t]
            try
                push!(item_dict[item_id], Item_Instance(auction_arr[i].contract_no , t, auction_arr[i], (auction_arr[i].num_bidders) ))
            catch error
               if isa(error, KeyError)
                    item_dict[item_id] = Set([Item_Instance(auction_arr[i].contract_no , t, auction_arr[i], (auction_arr[i].num_bidders))])
               end
            end

            item_cluster = auction_arr[i].item_clusters[t]
            try
                push!(item_cluster_dict[item_cluster], Item_Instance(auction_arr[i].contract_no , t, auction_arr[i], (auction_arr[i].num_bidders) ))
            catch error
               if isa(error, KeyError)
#                    println("No value for item_dict[$item_id]")
                    item_cluster_dict[item_cluster] = Set([Item_Instance(auction_arr[i].contract_no , t, auction_arr[i], (auction_arr[i].num_bidders))])
               end
            end
        end
    end

    item_counts = [auc.T for auc in auction_arr]
    max_T = maximum(item_counts)
    bidder_counts = [auc.num_bidders for auc in auction_arr]
    max_num_bidders = maximum(bidder_counts)

    num_proj_types = length(keys(project_type_dict))
    num_unique_items = length(keys(item_dict))
    num_auctions = length(auction_arr)
    num_bidder_features = length(auction_arr[1].bidders[1].X)
    num_unique_clusters = length(keys(item_cluster_dict))

    item_numobs_dict = Dict{Int64, Int64}()
    item_cluster_numobs_dict = Dict{Int64, Int64}()

    for item_id in keys(item_dict)
        num_item_obs = 0
        for item_inst in item_dict[item_id]
            num_item_obs = num_item_obs + item_inst.num_obs
        end
        item_numobs_dict[item_id] = num_item_obs
    end

    for item_cluster in keys(item_cluster_dict)
        num_item_obs = 0
        for item_inst in item_cluster_dict[item_cluster]
            num_item_obs = num_item_obs + item_inst.auction.num_bidders
        end
        item_cluster_numobs_dict[item_cluster] = num_item_obs
    end


    fringe_bidders = Set()
    unique_bidders = Set()
    for auc in auction_arr
        for n in 1:auc.num_bidders
            bidder_id = auc.bidders[n].bidder_id
            fringe_ind = auc.bidders[n].fringe_ind
            if(fringe_ind == 1)
                push!(fringe_bidders, bidder_id)
            else
                push!(unique_bidders, bidder_id)
            end
        end
    end

    num_unique_bidders = length(unique_bidders)
    fringe_id = num_unique_bidders + 1

    bidder_trunc_id_dict = Dict{Any, Any}()

    sequential_id = 0
    for auc in auction_arr
        for n in 1:auc.num_bidders

            bidder_id = auc.bidders[n].bidder_id
            bidder_fringe = auc.bidders[n].fringe_ind

            if(!(haskey(bidder_trunc_id_dict, bidder_id)))
                if(bidder_id in fringe_bidders)
                    bidder_trunc_id_dict[bidder_id] = fringe_id## This will be the index of all the fringe bidders
                else
                   bidder_trunc_id_dict[bidder_id] = (sequential_id+1) 
                    sequential_id = sequential_id + 1
                end

            end
        end
    end

    big_bidder_threshold = 100


    bidder_trunc_instance_dict = Dict{Any, Any}()

    for auc in auction_arr
        for n in 1:auc.num_bidders

            bidder_id = auc.bidders[n].bidder_id
            bidder_unique_id = bidder_trunc_id_dict[bidder_id]

            bidder_skew_item_indices = findall(x->x==1, auc.item_clusters)
            num_skew_items = length(bidder_skew_item_indices)

            try
                push!(bidder_trunc_instance_dict[bidder_unique_id], Bidder_Instance_w_Skew(auc.contract_no , n, auc, (auc.T), bidder_skew_item_indices, num_skew_items) )
                catch error
                   if isa(error, KeyError)
    #                    println("No value for item_dict[$item_id]")
                        bidder_trunc_instance_dict[bidder_unique_id] = Set([ Bidder_Instance_w_Skew(auc.contract_no , n, auc, (auc.T), bidder_skew_item_indices, num_skew_items) ])
                   end
            end
        end
    end

    bidder_trunc_numaucs_dict = Dict{Any, Int64}()
    for bidder_id in keys(bidder_trunc_instance_dict)
        num_bidder_obs = 0
        for bidder_inst in bidder_trunc_instance_dict[bidder_id]
            num_bidder_obs = num_bidder_obs + 1
        end
        bidder_trunc_numaucs_dict[bidder_id] = num_bidder_obs
    end


    bidder_trunc_numobs_dict = Dict{Any, Int64}()
    for bidder_id in keys(bidder_trunc_instance_dict)
        num_bidder_obs = 0
        for bidder_inst in bidder_trunc_instance_dict[bidder_id]
            num_bidder_obs = num_bidder_obs + bidder_inst.num_obs
        end
        bidder_trunc_numobs_dict[bidder_id] = num_bidder_obs
    end

    bidder_trunc_num_skew_obs_dict = Dict{Any, Int64}()
    for bidder_id in keys(bidder_trunc_instance_dict)
        num_bidder_obs = 0
        for bidder_inst in bidder_trunc_instance_dict[bidder_id]
            num_bidder_obs = num_bidder_obs + bidder_inst.num_skew_obs
        end
        bidder_trunc_num_skew_obs_dict[bidder_id] = num_bidder_obs
    end

    num_unique_truncated_bidders = num_unique_bidders + 1
    println("num_unique_truncated_bidders is ", num_unique_truncated_bidders)

    num_nus = sum( (sum(1 for t=1:auc.T) * auc.num_bidders) for auc in auction_arr)
    println("num_nus is ", num_nus)

    m = if USE_KNITRO
        Model(KNITRO.Optimizer)
    else
        Model(Ipopt.Optimizer)
    end

    @variables m begin
        inv_gamma_val[i = 1:num_unique_truncated_bidders]
        alpha_coeff[j = 1:num_bidder_features]
        alpha_bidder_val[i = 1:num_unique_truncated_bidders]
        inv_gamma_coeff[j = 1:num_bidder_features]
    end

    @NLexpression(
            m,
            alpha[auc in auction_arr, n in 1:auc.num_bidders],
            (sum(auc.bidders[n].X[j] * alpha_coeff[j] for j=1:num_bidder_features) + alpha_bidder_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]])
        )

    @NLexpression(
            m,
            inv_gamma[auc in auction_arr, n in 1:auc.num_bidders],
            (sum(auc.bidders[n].X[j] * inv_gamma_coeff[j] for j=1:num_bidder_features) + inv_gamma_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]])
        )


    @NLconstraint(
                m,
                [auc in auction_arr, n in 1:auc.num_bidders],
                alpha[auc,n] >= 0.5  
                )

    @NLconstraint(
                m,
                [auc in auction_arr, n in 1:auc.num_bidders],
                inv_gamma[auc,n] >= 2.0  
                )

    @NLconstraint(
                m,
                [auc in auction_arr, n in 1:auc.num_bidders],
                alpha[auc,n] <= 1.5  
                )


    @NLexpression(
            m,
            nu[auc in auction_arr, n in 1:auc.num_bidders,  t=1:auc.T],
            (auc.bidders[n].b_data[t] - (alpha[auc,n]*auc.optb_denom[t]) - (auc.optb_num_main[t]*inv_gamma[auc,n]) - auc.optb_num_coeff[t]*auc.bidders[n].score)
    )


    @NLexpression(
            m,
            moments2,
            sum( sum( sum( (nu[auc_inst.auction, auc_inst.bidder_seq_id, t]) for t in 1:auc_inst.auction.T) for auc_inst in bidder_trunc_instance_dict[bidder_id])^2 / bidder_trunc_numobs_dict[bidder_id] for bidder_id in keys(bidder_trunc_instance_dict))/num_unique_truncated_bidders
            )

    @NLexpression(
            m,
            moments3,
            sum( (sum( (nu[auc, n, t] * auc.bidders[n].X[j]) for auc in auction_arr, t in 1:auc.T, n in 1:auc.num_bidders)^2/num_nus) for j=1:num_bidder_features)/num_bidder_features
            )

    @NLexpression(
            m,
            moments4,
            sum( sum( sum( (nu[auc_inst.auction, auc_inst.bidder_seq_id, t]) for t in auc_inst.skew_item_ids) for auc_inst in bidder_trunc_instance_dict[bidder_id])^2 / bidder_trunc_num_skew_obs_dict[bidder_id] for bidder_id in keys(bidder_trunc_instance_dict))/num_unique_truncated_bidders
            )

    @NLexpression(
            m,
            moments5,
            sum( (sum( (sum( (nu[item_inst.auction, n, item_inst.seq_item_no] * item_inst.auction.bidders[n].X[j] ) for n in 1:item_inst.auction.num_bidders)/item_inst.auction.num_bidders) for item_inst in item_cluster_dict[1])^2 / item_cluster_numobs_dict[1]) for j=1:num_bidder_features)/num_bidder_features
            )

    @NLobjective(
                m,
                Min,
                moments2 +
                moments3 +
                moments4 +
                moments5
    )

    optimize!(m)

    println("Termination status: ", termination_status(m))
    println("Raw status:         ", raw_status(m))

    answer = objective_value.(m)
    println("answer: ", answer)
    
    alpha_coeff_min = [value.(alpha_coeff[j]) for j = 1:num_bidder_features]
    inv_gamma_coeff_min = [value.(inv_gamma_coeff[j]) for j = 1:num_bidder_features]

    N_I = 0
    gamma_dict = Dict{}()
    alpha_dict = Dict{}()
    for auc in auction_arr, n in range(1,stop = auc.num_bidders)
        alpha_est = (sum(auc.bidders[n].X[j] * alpha_coeff_min[j] for j=1:num_bidder_features) + value.(alpha_bidder_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]]))
        gamma_est = 1.0/(sum(auc.bidders[n].X[j] * inv_gamma_coeff_min[j] for j=1:num_bidder_features) + value.(inv_gamma_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]]))

        alpha_dict[auc, n] = alpha_est
        gamma_dict[auc, n] = gamma_est
        N_I = N_I + 1
    end

    function getBidderMarkup(b, q_a, c, alpha)
        bidder_cost = (alpha*c)' * q_a
        bidder_rev = b' * q_a

        markup = (bidder_rev - bidder_cost)/bidder_cost

        return (bidder_cost, bidder_rev, markup)
    end

    winning_mkps = [1.2 for i in 1:length(auction_arr)]
    num_bidders = [0 for i in 1:length(auction_arr)]

    alphas = [1.2 for i = 1:N_I]
    gammas = [1.2 for i = 1:N_I]
    mkps = [1.2 for i = 1:N_I]
    num_bidders = [0 for i = 1:N_I]
    contract_nos = [0 for i = 1:N_I]
    bidder_ids = ["a" for i = 1:N_I]

    i = 0
    for auc in auction_arr, n in 1:auc.num_bidders
        i= i+1
        alpha_est = alpha_dict[auc, n]
        gamma_est = gamma_dict[auc, n]
        mkp_tuple = getBidderMarkup(auc.bidders[n].b_data, auc.q_a, auc.c, alpha_est)

        alphas[i] = alpha_est
        mkps[i] = mkp_tuple[3]
        num_bidders[i] = auc.num_bidders
        contract_nos[i] = auc.contract_no
        bidder_ids[i] = auc.bidders[n].bidder_id
        gammas[i] = gamma_est
    end

    mkp_df = DataFrame(gamma = gammas, alpha = alphas, mkp = mkps, num_bids = num_bidders, contract_no = contract_nos, bidder_id = bidder_ids)

    CSV.write(joinpath(out_dir, "$sample_no" * "_mkp_df.csv"), mkp_df)

    describe(mkp_df)

    num_unique_bidder_ids = length(keys(bidder_trunc_id_dict))
    alpha_i = [1.2 for i = 1:(num_unique_bidder_ids)]
    bidder_i = ["" for i = 1:(num_unique_bidder_ids)]

    alpha_coeff_min = [value.(alpha_coeff[j]) for j = 1:num_bidder_features]

#     j = 0
#     for bidder in keys(bidder_trunc_id_dict) 
#         global j = j+1
#         bidder_i[j] = bidder
#         alpha_i[j] = value(alpha_bidder_val[bidder_trunc_id_dict[bidder]])
#     end
    
    local j = 0
    for bidder in keys(bidder_trunc_id_dict) #[auc.bidders[n].bidder_id]
            j = j+1
            bidder_i[j] = bidder
            alpha_i[j] = value(alpha_bidder_val[bidder_trunc_id_dict[bidder]])
    end

    
    alpha_i_df = DataFrame(bidder_id = bidder_i, alpha_i = alpha_i)
    CSV.write(joinpath(out_dir, "$sample_no" * "_alpha_i.csv"), alpha_i_df)

    beta_df = DataFrame(beta = alpha_coeff_min)
    CSV.write(joinpath(out_dir, "$sample_no" * "_beta_alpha.csv"), beta_df)
    
    num_unique_bidder_ids = length(keys(bidder_trunc_id_dict))
    gamma_i = [1.2 for i = 1:(num_unique_bidder_ids)]
    bidder_i = ["" for i = 1:(num_unique_bidder_ids)]

    inv_gamma_coeff_min = [value.(inv_gamma_coeff[j]) for j = 1:num_bidder_features]

#     j = 0
#     for bidder in keys(bidder_trunc_id_dict) 
#         global j = j+1
#         bidder_i[j] = bidder
#         gamma_i[j] = value(inv_gamma_val[bidder_trunc_id_dict[bidder]])
#     end
    
    local j = 0
    for bidder in keys(bidder_trunc_id_dict) #[auc.bidders[n].bidder_id]
            j = j+1
            bidder_i[j] = bidder
            gamma_i[j] = value(inv_gamma_val[bidder_trunc_id_dict[bidder]])
    end


    gamma_i_df = DataFrame(bidder_id = bidder_i, gamma_i = gamma_i)
    CSV.write(joinpath(out_dir, "$sample_no" * "_gamma_i.csv"), gamma_i_df)

    beta_g_df = DataFrame(beta = inv_gamma_coeff_min)
    CSV.write(joinpath(out_dir, "$sample_no" * "_beta_gamma.csv"), beta_g_df)

end