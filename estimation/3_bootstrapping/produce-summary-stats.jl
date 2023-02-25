# Import packages
import Pkg;
Pkg.activate(@__DIR__);
using JuMP
using Ipopt
using DataFrames, CSV
using Dates
using Ipopt, KNITRO

# Set this to true to use KNITRO, if it is available.
# KNITRO is significantly faster than Ipopt.
const USE_KNITRO=false

# Set up directories
project_dir = dirname(dirname(@__DIR__))
julia_data_dir = joinpath(project_dir, "data", "estimation_step2_inputs", "full/")
out_dir = joinpath(project_dir, "data", "estimation_step3/")

# Preliminary structs
mutable struct Bidder
    bidder_seq_id::Int64
    bidder_id::String
    b_data::Array{Float64}
    score::Float64
    X::Array{Float64}
    fringe_ind::Int64
end

mutable struct MinimalAuction
    contract_no::Int64
    project_type_id::Int64
    num_bidders::Int64
    T::Int64
    bidders::Array{Bidder}
    optb_denom::Array{Float64}
    optb_num_coeff::Array{Float64}
    optb_num_main::Array{Float64}
    sum_qesq_sigsq::Float64
    sum_qe_co::Float64
    sum_qe_qa_sigsq::Float64
    q_a::Array{Float64}
    q_a_model::Array{Float64}
    q_o::Array{Float64}
    c::Array{Float64}
    sigma_sq::Array{Float64}
    H_coef1::Float64
    H_coef2::Float64
    H_exponent::Float64
    office_score::Float64
    ewo::Float64
    item_ids::Array{Int64}
    item_clusters::Array{Int64}
end

mutable struct Item_Instance
    contract_no::Int64 ## maps auctions the item is included in to the item's position
    seq_item_no::Int64
    auction::MinimalAuction
    num_obs::Int64
end

mutable struct Bidder_Instance
    contract_no::Int64 ## maps auctions the item is included in to the item's position
    bidder_seq_id::Int64
    auction::MinimalAuction
    num_obs::Int64
end

mutable struct Bidder_Instance_w_Skew
    contract_no::Int64 ## maps auctions the item is included in to the item's position
    bidder_seq_id::Int64
    auction::MinimalAuction
    num_obs::Int64
    skew_item_ids::Array{Float64}
    num_skew_obs::Int64
end

function constructMinimalAuction(
    auction_data::DataFrames.DataFrame, 
    bidder_data::DataFrames.DataFrame, 
    bidder_type_data::DataFrames.DataFrame, 
    dollar_scale::Int64
)
    contract_no = auction_data[!, :contract_no][1]
    project_type_id = auction_data[!, :project_type_id][1]

    q_a_model = convert(Array{Float64}, auction_data[!, :q_at_model])
    q_e = convert(Array{Float64}, auction_data[!, :q_ot])
    q_a = convert(Array{Float64}, auction_data[!, :q_at])
    c_o = convert(Array{Float64}, auction_data[!, :office_unit_price] / dollar_scale)
    sigma_t = convert(Array{Float64}, auction_data[!, :sigma_t])

    item_ids = convert(Array{Float64}, bidder_data[!, :item_id_sequential])
    item_clusters = convert(Array{Float64}, bidder_data[!, :skew_cluster])

    ewo = convert(Array{Float64}, auction_data[!, :extra_work_payment])[1]
    ewo = ewo / dollar_scale

    T = length(q_e)
    sigma_sq = sigma_t .* sigma_t

    sum_qesq_sigsq = sum((q_e[t]^2) / sigma_sq[t] for t in 1:T)
    sum_qe_co = sum((q_e[t] * c_o[t]) for t in 1:T)
    sum_qe_qa_sigsq = sum((q_e[t] * q_a_model[t]) / sigma_sq[t] for t in 1:T)

    denom = [c_o[t] - (q_e[t] / (sigma_sq[t] * sum_qesq_sigsq)) * sum_qe_co for t in 1:T]
    num_coeff = [q_e[t] / (sigma_sq[t] * sum_qesq_sigsq) for t in 1:T]
    num_main = [(q_a_model[t] / sigma_sq[t]) - (num_coeff[t] * sum_qe_qa_sigsq) for t in 1:T]

    num_bidders = ncol(bidder_data) - 2 ## Accounting for new 'generic item ids'
    num_bidder_feat_cols = ncol(bidder_type_data)

    bidders = [Bidder(
        (i - 2),
        string(names(bidder_data)[i]),
        convert(Array{Float64}, bidder_data[:, i] / dollar_scale),
        (((bidder_data[:, i])' * q_e) / dollar_scale),
        (permutedims(Vector(bidder_type_data[(3-2), 4:num_bidder_feat_cols]))[1, :]), ## need to do df row to array conversion this way
        bidder_type_data[(i-2), 3]
    ) for i = 3:(num_bidders+2)]

    bidders = sort!(bidders, by=x -> x.score)
    for i in 1:length(bidders)
        bidders[i].bidder_seq_id = i
    end

    ## compute coefficients for hazard rate approximation
    phi_approx_a = 1.702

    bidder_auction_mean = convert(Array{Float64}, auction_data[!, :bidder_auction_mean])[1]
    bidder_auction_sigma_k = convert(Array{Float64}, auction_data[!, :bidder_auction_sigma_k])[1]

    office_score = (c_o' * q_e) / dollar_scale

    H_coef1 = phi_approx_a / (bidder_auction_sigma_k * office_score)
    H_coef2 = exp((phi_approx_a * bidder_auction_mean) / bidder_auction_sigma_k) * (office_score)^(-phi_approx_a / bidder_auction_sigma_k)
    H_exponent = (-phi_approx_a) / bidder_auction_sigma_k

    return MinimalAuction(
        contract_no,
        project_type_id,
        num_bidders,
        T,
        bidders,
        denom,
        num_coeff,
        num_main,
        sum_qesq_sigsq,
        sum_qe_co,
        sum_qe_qa_sigsq,
        q_a,
        q_a_model,
        q_e,
        c_o,
        sigma_sq,
        H_coef1,
        H_coef2,
        H_exponent,
        office_score,
        ewo,
        item_ids,
        item_clusters)
end

contract_list = [30163, 30230, 31050, 31063, 31082, 31084, 31172, 31200, 31267, 31292, 31296, 31348, 31353, 31356, 31358, 31359, 32029, 32045, 32052, 32075, 32080, 32087, 32096, 32109, 32126, 32130, 32160, 32189, 32192, 32200, 32220, 32253, 32264, 32270, 32284, 32286, 32292, 32301, 33000, 33025, 33064, 33065, 33067, 33068, 33122, 33126, 33144, 33152, 33153, 33161, 33199, 33201, 33203, 33237, 33238, 33251, 33253, 33260, 33272, 33276, 33278, 33282, 33305, 33316, 33319, 34004, 34034, 34036, 34043, 34056, 34059, 34449, 34450, 34451, 34452, 34460, 34485, 34486, 34489, 34508, 34510, 34520, 34531, 34548, 34569, 34584, 34585, 34594, 34600, 34612, 34636, 34644, 34653, 34668, 34701, 35032, 35035, 35036, 35657, 35842, 36127, 36912, 37671, 38095, 38122, 38638, 38807, 38900, 39036, 39147, 39651, 39741, 39742, 39899, 39932, 39967, 41052, 41064, 41067, 41198, 41586, 41852, 41863, 42088, 42097, 42177, 42308, 42404, 42405, 42409, 42616, 42947, 43123, 43326, 43332, 43482, 43627, 43675, 43702, 43781, 43821, 44007, 44013, 44196, 44339, 44341, 44385, 44828, 45142, 45263, 45409, 45411, 45509, 45564, 45713, 48016, 48023, 48168, 48688, 49428, 49429, 49437, 49586, 51216, 51231, 51326, 51392, 51409, 51588, 51612, 51676, 51773, 51785, 51847, 52030, 52128, 52511, 52513, 52649, 52698, 52805, 52871, 53243, 53270, 53347, 53408, 53414, 53472, 53482, 53491, 53517, 53521, 53558, 53567, 53865, 53915, 53932, 53966, 54020, 54045, 54172, 54173, 54244, 54278, 54336, 54360, 54559, 54707, 54747, 54992, 55026, 55339, 55488, 55538, 55679, 55843, 55855, 55981, 56067, 56216, 56387, 56482, 56515, 56557, 56580, 56608, 56636, 56745, 56746, 56748, 56750, 56779, 56836, 56846, 56859, 56993, 56995, 57045, 57162, 57182, 57227, 57275, 57342, 57343, 57344, 57381, 57419, 57452, 57455, 57636, 57640, 57641, 57664, 57675, 57676, 57757, 57759, 57804, 57889, 57962, 57963, 58005, 58007, 58045, 58047, 58085, 58134, 58658, 58901, 59049, 59130, 59245, 59675, 59676, 59677, 59772, 59773, 59934, 59935, 59936, 60073, 60075, 60150, 60195, 60198, 60310, 60311, 60370, 60371, 60374, 60857, 61463, 61793, 61912, 61916, 61941, 62025, 62564, 62672, 62908, 62909, 63233, 63234, 63235, 63236, 63317, 63700, 63701, 63793, 63796, 63977, 63978, 64519, 64522, 64843, 64848, 65190, 65274, 65281, 65441, 65602, 65605, 65709, 65714, 65827, 65951, 65952, 65956, 66086, 66090, 66091, 66333, 66336, 66457, 66460, 66461, 66466, 66564, 66755, 66834, 66838, 66843, 67019, 67020, 67022, 67059, 67099, 67215, 67220, 67308, 67311, 67313, 67314, 67528, 67530, 67664, 67756, 67757, 67759, 67813, 67817, 67947, 68026, 68135, 68145, 68155, 68164, 68166, 68171, 68186, 68187, 68209, 68223, 68305, 68373, 68530, 68531, 69940, 70213, 70380, 70720, 70723, 70726, 70936, 71085, 71090, 71092, 71095, 71101, 71225, 71228, 72260, 72306, 72313, 72315, 72560, 72562, 73931, 74405, 75231, 75232, 75236, 75351, 75353, 75356, 75581, 75582, 75584, 75704, 75772, 76248, 76401, 76477, 76731, 76936, 76939, 78187, 78541, 79145, 79291, 79293, 79294, 79399, 79512, 79519, 79598, 79732, 79737, 79743, 79871, 79881, 80194, 84362, 99030, 99175, 99179, 99185, 99242]
## excluding: 63232 and 71098

"""
    go()

Run the main function. This function will read in the data and produce the file
    
    `data/estimation_step3/second_stage_estimates_summary.csv`
"""
function go()
    num_contracts = length(contract_list)
    auction_arr = Vector{MinimalAuction}(undef, num_contracts)
    item_dict = Dict{Int64,Any}()
    item_cluster_dict = Dict{Int64,Any}()

    auc_arr_contract_nos = Vector{Int64}(undef, num_contracts)
    project_type_dict = Dict{Int64,Int64}() #dictionary mapping project type ids to the number of contracts of that type
    dollar_scale = 1000


    for i = 1:num_contracts
        contract = contract_list[i]
        example_project = DataFrame(CSV.File(julia_data_dir * "sample_project_$contract.csv"))
        example_bidders = DataFrame(CSV.File(julia_data_dir * "sample_project_bidders_$contract.csv"))
        example_bidder_types = DataFrame(CSV.File(julia_data_dir * "sample_project_biddertypes_$contract.csv"))

        auction_arr[i] = constructMinimalAuction(example_project, example_bidders, example_bidder_types, dollar_scale)
        auc_arr_contract_nos[i] = auction_arr[i].contract_no
        auc_project_type_id = auction_arr[i].project_type_id

        try
            project_type_dict[auc_project_type_id] = project_type_dict[auc_project_type_id] + 1
        catch error
            if isa(error, KeyError)
                project_type_dict[auc_project_type_id] = 1
            end
        end

        for t in 1:auction_arr[i].T
            item_id = auction_arr[i].item_ids[t]
            try
                push!(item_dict[item_id], Item_Instance(auction_arr[i].contract_no, t, auction_arr[i], (auction_arr[i].num_bidders)))
            catch error
                if isa(error, KeyError)
                    item_dict[item_id] = Set([Item_Instance(auction_arr[i].contract_no, t, auction_arr[i], (auction_arr[i].num_bidders))])
                end
            end

            item_cluster = auction_arr[i].item_clusters[t]
            try
                push!(item_cluster_dict[item_cluster], Item_Instance(auction_arr[i].contract_no, t, auction_arr[i], (auction_arr[i].num_bidders)))
            catch error
                if isa(error, KeyError)
                    item_cluster_dict[item_cluster] = Set([Item_Instance(auction_arr[i].contract_no, t, auction_arr[i], (auction_arr[i].num_bidders))])
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

    fringe_bidders = Set()
    unique_bidders = Set()
    for auc in auction_arr
        for n in 1:auc.num_bidders
            bidder_id = auc.bidders[n].bidder_id
            fringe_ind = auc.bidders[n].fringe_ind
            if (fringe_ind == 1)
                push!(fringe_bidders, bidder_id)
            else
                push!(unique_bidders, bidder_id)
            end
        end
    end

    print(length(unique_bidders))

    item_numobs_dict = Dict{Int64,Int64}()
    item_cluster_numobs_dict = Dict{Int64,Int64}()

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

    item_cluster_numobs_dict

    fringe_bidders = Set()
    unique_bidders = Set()
    for auc in auction_arr
        for n in 1:auc.num_bidders
            bidder_id = auc.bidders[n].bidder_id
            fringe_ind = auc.bidders[n].fringe_ind
            if (fringe_ind == 1)
                push!(fringe_bidders, bidder_id)
            else
                push!(unique_bidders, bidder_id)
            end
        end
    end

    num_unique_bidders = length(unique_bidders)
    fringe_id = num_unique_bidders + 1

    bidder_trunc_id_dict = Dict{Any,Any}()

    sequential_id = 0
    for auc in auction_arr
        for n in 1:auc.num_bidders

            bidder_id = auc.bidders[n].bidder_id
            bidder_fringe = auc.bidders[n].fringe_ind

            if (!(haskey(bidder_trunc_id_dict, bidder_id)))
                if (bidder_id in fringe_bidders)
                    bidder_trunc_id_dict[bidder_id] = fringe_id## This will be the index of all the fringe bidders
                else
                    bidder_trunc_id_dict[bidder_id] = (sequential_id + 1)
                    sequential_id = sequential_id + 1
                end

            end
        end
    end

    big_bidder_threshold = 100


    bidder_trunc_instance_dict = Dict{Any,Any}()

    for auc in auction_arr
        for n in 1:auc.num_bidders

            bidder_id = auc.bidders[n].bidder_id
            bidder_unique_id = bidder_trunc_id_dict[bidder_id]

            bidder_skew_item_indices = findall(x -> x == 1, auc.item_clusters)
            num_skew_items = length(bidder_skew_item_indices)

            try
                push!(bidder_trunc_instance_dict[bidder_unique_id], Bidder_Instance_w_Skew(auc.contract_no, n, auc, (auc.T), bidder_skew_item_indices, num_skew_items))
            catch error
                if isa(error, KeyError)
                    bidder_trunc_instance_dict[bidder_unique_id] = Set([Bidder_Instance_w_Skew(auc.contract_no, n, auc, (auc.T), bidder_skew_item_indices, num_skew_items)])
                end
            end
        end
    end

    bidder_trunc_numaucs_dict = Dict{Any,Int64}()
    for bidder_id in keys(bidder_trunc_instance_dict)
        num_bidder_obs = 0
        for bidder_inst in bidder_trunc_instance_dict[bidder_id]
            num_bidder_obs = num_bidder_obs + 1
        end
        bidder_trunc_numaucs_dict[bidder_id] = num_bidder_obs
    end


    bidder_trunc_numobs_dict = Dict{Any,Int64}()
    for bidder_id in keys(bidder_trunc_instance_dict)
        num_bidder_obs = 0
        for bidder_inst in bidder_trunc_instance_dict[bidder_id]
            num_bidder_obs = num_bidder_obs + bidder_inst.num_obs
        end
        bidder_trunc_numobs_dict[bidder_id] = num_bidder_obs
    end

    bidder_trunc_num_skew_obs_dict = Dict{Any,Int64}()
    for bidder_id in keys(bidder_trunc_instance_dict)
        num_bidder_obs = 0
        for bidder_inst in bidder_trunc_instance_dict[bidder_id]
            num_bidder_obs = num_bidder_obs + bidder_inst.num_skew_obs
        end
        bidder_trunc_num_skew_obs_dict[bidder_id] = num_bidder_obs
    end

    num_unique_truncated_bidders = num_unique_bidders + 1
    println("num_unique_truncated_bidders is ", num_unique_truncated_bidders)

    num_nus = sum((sum(1 for t = 1:auc.T) * auc.num_bidders) for auc in auction_arr)
    println("num_nus is ", num_nus)

    # m = Model(Ipopt.Optimizer)
    # m = Model(KNITRO.Optimizer)
    m = if USE_KNITRO
        Model(KNITRO.Optimizer)
    else
        Model(Ipopt.Optimizer)
    end


    @variables m begin
        inv_gamma_val[i=1:num_unique_truncated_bidders]
        alpha_coeff[j=1:num_bidder_features]
        alpha_bidder_val[i=1:num_unique_truncated_bidders]
        inv_gamma_coeff[j=1:num_bidder_features]
    end

    @NLexpression(
        m,
        alpha[auc in auction_arr, n in 1:auc.num_bidders],
        (sum(auc.bidders[n].X[j] * alpha_coeff[j] for j = 1:num_bidder_features) + alpha_bidder_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]])
    );

    @NLexpression(
        m,
        inv_gamma[auc in auction_arr, n in 1:auc.num_bidders],
        (sum(auc.bidders[n].X[j] * inv_gamma_coeff[j] for j = 1:num_bidder_features) + inv_gamma_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]])
    );


    @NLconstraint(
        m,
        [auc in auction_arr, n in 1:auc.num_bidders],
        alpha[auc, n] >= 0.5
    );

    @NLconstraint(
        m,
        [auc in auction_arr, n in 1:auc.num_bidders],
        inv_gamma[auc, n] >= 2.0
    );

    @NLconstraint(
        m,
        [auc in auction_arr, n in 1:auc.num_bidders],
        alpha[auc, n] <= 1.5
    );


    @NLexpression(
        m,
        nu[auc in auction_arr, n in 1:auc.num_bidders, t=1:auc.T],
        (auc.bidders[n].b_data[t] - (alpha[auc, n] * auc.optb_denom[t]) - (auc.optb_num_main[t] * inv_gamma[auc, n]) - auc.optb_num_coeff[t] * auc.bidders[n].score)
    );


    @NLexpression(
        m,
        moments2,
        sum(sum(sum((nu[auc_inst.auction, auc_inst.bidder_seq_id, t]) for t in 1:auc_inst.auction.T) for auc_inst in bidder_trunc_instance_dict[bidder_id])^2 / bidder_trunc_numobs_dict[bidder_id] for bidder_id in keys(bidder_trunc_instance_dict)) / num_unique_truncated_bidders
    );

    @NLexpression(
        m,
        moments3,
        sum((sum((nu[auc, n, t] * auc.bidders[n].X[j]) for auc in auction_arr, t in 1:auc.T, n in 1:auc.num_bidders)^2 / num_nus) for j = 1:num_bidder_features) / num_bidder_features
    );

    @NLexpression(
        m,
        moments4,
        sum(sum(sum((nu[auc_inst.auction, auc_inst.bidder_seq_id, t]) for t in auc_inst.skew_item_ids) for auc_inst in bidder_trunc_instance_dict[bidder_id])^2 / bidder_trunc_num_skew_obs_dict[bidder_id] for bidder_id in keys(bidder_trunc_instance_dict)) / num_unique_truncated_bidders
    );

    @NLexpression(
        m,
        moments5,
        sum((sum((sum((nu[item_inst.auction, n, item_inst.seq_item_no] * item_inst.auction.bidders[n].X[j]) for n in 1:item_inst.auction.num_bidders) / item_inst.auction.num_bidders) for item_inst in item_cluster_dict[1])^2 / item_cluster_numobs_dict[1]) for j = 1:num_bidder_features) / num_bidder_features
    );

    @NLobjective(
        m,
        Min,
        moments2 +
        moments3 +
        moments4 +
        moments5
    );

    optimize!(m)

    answer = objective_value.(m)
    println("answer: ", answer)

    alpha_coeff_min = [value.(alpha_coeff[j]) for j = 1:num_bidder_features]
    inv_gamma_coeff_min = [value.(inv_gamma_coeff[j]) for j = 1:num_bidder_features]

    N_I = 0
    gamma_dict = Dict{}()
    alpha_dict = Dict{}()
    for auc in auction_arr, n in range(1, stop=auc.num_bidders)
        alpha_est = (sum(auc.bidders[n].X[j] * alpha_coeff_min[j] for j = 1:num_bidder_features) + value.(alpha_bidder_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]]))
        gamma_est = 1.0 / (sum(auc.bidders[n].X[j] * inv_gamma_coeff_min[j] for j = 1:num_bidder_features) + value.(inv_gamma_val[bidder_trunc_id_dict[auc.bidders[n].bidder_id]]))

        alpha_dict[auc, n] = alpha_est
        gamma_dict[auc, n] = gamma_est
        N_I = N_I + 1
    end

    function getBidderMarkup(b, q_a, c, alpha)
        bidder_cost = (alpha * c)' * q_a
        bidder_rev = b' * q_a

        markup = (bidder_rev - bidder_cost) / bidder_cost

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
    bidder_cost = [1.2 for i = 1:N_I]
    bidder_rev = [1.2 for i = 1:N_I]

    i = 0
    for auc in auction_arr, n in 1:auc.num_bidders
        i = i + 1
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


    mkp_df = DataFrame(gamma=gammas, alpha=alphas, mkp=mkps, num_bids=num_bidders, contract_no=contract_nos, bidder_id=bidder_ids)

    # Save the file
    today = Dates.today()
    CSV.write(out_dir * "second_stage_estimates_summary.csv", mkp_df)

    ## Get v_bar
    nu_dict = Dict{Any, Any}()

    N_I = 0

    for auc in auction_arr, n in range(1,stop = auc.num_bidders)
        inv_gamma_est = 1.0/gamma_dict[auc, n]
        alpha_est = alpha_dict[auc, n]
        
        nu_dict[auc.bidders[n]]  = [(auc.bidders[n].b_data[t] - (alpha_est*auc.optb_denom[t]) - (auc.optb_num_main[t]*inv_gamma_est) - auc.optb_num_coeff[t]*auc.bidders[n].score) for t = 1:auc.T]

        N_I = N_I + 1
    end

    bid_fit_vec = [1.2 for t=1:num_nus]
    bid_fit_w_error_vec = [1.2 for t=1:num_nus]
    nu_arr = [1.2 for t=1:num_nus]


    contract_nos = [0 for t = 1:num_nus]
    bidder_ids = ["a" for t = 1:num_nus]
    alphas = [1.2 for t = 1:num_nus]
    invgammas = [1.2 for t = 1:num_nus]
    invgamma_coeffs = [1.2 for t = 1:num_nus]
    num_item = [1.2 for t = 1:num_nus]
    item_ids = [1.2 for t = 1:num_nus];

    ## Get v_bar

    t_start = 1
    for auc in auction_arr
        for n in 1:auc.num_bidders

            inv_gamma_est = 1.0/gamma_dict[auc, n]
            alpha_est = alpha_dict[auc, n]
            
            t_end = t_start + auc.T - 1
            bid_fit_vec[t_start:t_end]  = [ ((alpha_est*auc.optb_denom[t]) + (auc.optb_num_main[t]*inv_gamma_est) + auc.optb_num_coeff[t]*auc.bidders[n].score) for t = 1:auc.T]
            bid_fit_w_error_vec[t_start:t_end]  = [ ( (alpha_est*auc.optb_denom[t]) + (auc.optb_num_main[t]*inv_gamma_est) + auc.optb_num_coeff[t]*(auc.bidders[n].score - sum( auc.q_o[t]*nu_dict[auc.bidders[n]][t] for t in 1:auc.T))) for t = 1:auc.T]

            nu_arr[t_start:t_end] = [nu_dict[auc.bidders[n]][t] for t = 1:auc.T]

            invgamma_coeffs[t_start:t_end] = [auc.optb_num_main[t] for t = 1:auc.T]
            contract_nos[t_start:t_end] = [auc.contract_no for t = 1:auc.T]
            bidder_ids[t_start:t_end] = [auc.bidders[n].bidder_id for t = 1:auc.T]
            alphas[t_start:t_end] = [alpha_est for t = 1:auc.T]
            invgammas[t_start:t_end] = [inv_gamma_est for t = 1:auc.T]
            num_item[t_start:t_end] = [auc.T for t = 1:auc.T]

            item_ids[t_start:t_end] = [auc.item_ids[t] for t = 1:auc.T]


            t_start = t_end + 1
        end
    end

    bid_df = DataFrame(contract_no = contract_nos, bidder_id = bidder_ids, inv_gamma = invgammas, alpha = alphas, bid_fit = bid_fit_vec, bid_fit_w_error = bid_fit_w_error_vec, invgamma_coeff = invgamma_coeffs, item_id = item_ids, num_items = num_item, nu = nu_arr)
    CSV.write(out_dir * "second_stage_estimates_bid_fit.csv", bid_df)

    return mkp_df
end

# Run the code
mkp_df = go()