function load_auctions(
    data_path,
    out_path,
    cf_name;
)
    # Grab the date
    today = Dates.today()

    # Paths
    in_path = joinpath(data_path, cf_name * ".csv")
    in_dir = joinpath(data_path)
    out_dir = joinpath(out_path, "output", string(today), cf_name)

    !ispath(out_dir) && mkpath(out_dir)

    # Get auction data
    println("Getting cf_data alpha_data")
    cf_data = DataFrame(CSV.File(in_path, types=Dict(:pois_gamma_projfe => Float64)))
    # bin_data = DataFrame(CSV.File(joinpath(in_dir, "alpha_gamma_distribution_v6.csv")))
    alpha_data = DataFrame(CSV.File(joinpath(in_dir, "alphas_by_auction_with_features.csv")))
    # win_data = DataFrame(CSV.File(joinpath(in_dir, "mh_input_binlevel_60015259.csv")))
    # select!(win_data, [:contract_no, :winning_alpha_eval])

    # cf_data = leftjoin(cf_data, win_data, on=:contract_no)

    # Add bin information to each contract
    # println("Combining cf_data & bin_data")
    # display(bin_data)
    # display(cf_data)
    # cf_data = leftjoin(
    #     select(cf_data),
    #     bin_data,
    #     on=intersect(names(cf_data), names(bin_data))
    # )

    # cf_data = leftjoin(
    #     cf_data,
    #     alpha_data,
    #     on=intersect(names(cf_data), names(alpha_data))
    # )

    println("uniqifying cf_Data")
    unique!(cf_data)

    num_contracts = nrow(cf_data)
    @info "" num_contracts
    auction_arr = Vector{Auction}(undef, num_contracts)

    for row in 1:num_contracts
        project_df = cf_data[row,:]
        contract = project_df[:contract_no][1]

        auction_arr[row] = construct_auction(in_dir, project_df, alpha_data)

        # project = DataFrame(CSV.File(joinpath(in_dir, "sample_project_$contract.csv")))
        # bidders = DataFrame(CSV.File(joinpath(in_dir, "sample_project_bidders_$contract.csv")))

        # auction_arr[row] = construct_auction(project, bidders)
    end

    println("Extracting instruments")
    contracts = map(x -> x.contract_no, auction_arr)
    managers = map(x -> x.proj_manager_id, auction_arr)
    engineers = map(x -> x.engineer_id, auction_arr)
    designers = map(x -> x.designer_id, auction_arr)

    println("manager/engineer indicatormat")
    manager_instruments = map(identity, Array(indicatormat(managers)'))
    engineer_instruments = map(identity, Array(indicatormat(engineers)'))

    println("Making Z")
    Z = DataFrame(hcat(manager_instruments, engineer_instruments), :auto)
    Z[!, :contract_no] = contracts
    Z = leftjoin(select(cf_data, [:contract_no, :potential_bidder_bin_id]), Z, on=:contract_no)

    println("Taking average of Z")
    # display(Z)
    # Z2 = combine(
    #     groupby(select(Z, Not(:contract_no)), :potential_bidder_bin_id),
    #     All() .=> mean
    # )

    bin_ids = unique(Z.potential_bidder_bin_id)
    group_data = zeros((length∘unique)(Z.potential_bidder_bin_id), size(Z,2))

    for bin_id in bin_ids
        subdf = Array(filter(x -> x.potential_bidder_bin_id == bin_id, Z))
        group_data[bin_id, :] = mean(subdf, dims=1)
    end

    # Z2 = DataFrame(group_data, )

    println("Making z2_arr")
    #Z2_bins = Z2.potential_bidder_bin_id
    #select!(Z2, Not([:potential_bidder_bin_id_mean, :potential_bidder_bin_id]))
    Z2_arr = group_data

    println("returning")
    return auction_arr, Z, Z2_arr
end

trim_y_bar(ys; cap=inf) = map(y -> min(y, cap), ys)

function calc_y_bar(auc, estimated; cap=Inf, n_items=1)
    # Short circuit all this garbage
    # return zeros(auc.T) .+ 0.1
    actual = auc.q_a
    estimated = auc.q_o

    bidders = sort(auc.bidders, by = x -> x.score)
    top_bidder = bidders[1]
    top_bids = top_bidder.b
    overbid = top_bids .<= auc.winning_alpha .* auc.c

    y_bar = zeros(auc.T)
    for i in 1:auc.T
        if overbid[i]
            y_bar[i] = estimated[i] == 0 ? 0 : max(0, actual[i] / estimated[i] - 1)
        else
            # y_bar[i] is zero at this point, do nothing
        end
    end

    # Find the top n_items, set all others to zero.
    locs = map(i -> (i, y_bar[i]), 1:auc.T)
    sort!(locs, by = m -> m[2], rev = true)
    top_inds = map(xx -> xx[1], locs[1:n_items])
    for i in 1:auc.T
        if i in top_inds
            # Do nothing
        else
            y_bar[i] = 0.0
        end
    end

    y_bar = trim_y_bar(y_bar, cap=cap)

    # println("Bidding information")
    # display(DataFrame(
    #     bids=top_bids,
    #     cost=auc.winning_alpha .* auc.c,
    #     overbid=overbid,
    #     actual=actual,
    #     estimated=estimated,
    #     y_bar=y_bar
    # ))
    @assert all(y_bar .>= 0)

    return y_bar
end
