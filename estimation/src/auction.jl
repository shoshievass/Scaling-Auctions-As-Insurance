mutable struct Bidder
    bidder_id::String
    b::Vector{Float64}
    score::Float64
end

mutable struct Auction{B,G,A}
    contract_no::Int64
    num_bidders::Int64
    T::Int64
    bidders::B
    q_a::Vector{Float64} # Actual
    q_a_model::Vector{Float64} # Predicted
    q_o::Vector{Float64} # Engineer estimated
    c::Vector{Float64}
    sigma_t_sq::Vector{Float64}
    gamma::G
    item_ids::Vector{Int64}
    extra_work_payment::Float64
    winning_alpha::Union{Float64,Missing}
    highest_alpha::Float64
    lowest_alpha::Float64
    alpha_lgmean::Float64
    alpha_lgsigma::Float64
    dollar_scale::Int
    alpha_coef::A
    auction_fe::A
    proj_manager_id::Int
    engineer_id::Int
    designer_id::Int
    alphas::Vector{Float64}
    alpha_bar::Float64
    alpha_bar_bar::Float64
    max_num_bidders::Int
    bin_id::Int
    alpha_min_bin::Float64
end

function hascol(col, df::DataFrame)
    return col in names(df)
end

function construct_auction(in_dir, data, alpha_data)
    # Top-level data
    dollar_scale = 1_000 # data[:dollar_scale][1]
    contract_no = data[:contract_no][1]
    num_bidders = data[:num_bids][1]
    extra_work_payment = data[:extra_work_payment][1] / dollar_scale
    # winning_alpha = missing
    winning_alpha = data[:winning_alpha_eval][1]

    highest_alpha_raw = data[:max_alpha_seen][1]
    lowest_alpha_raw = (0.66 * data[:alpha_min][1]) # data[:min_alpha_seen][1] changing this to a bin-wide min with a buffer
    highest_alpha = min(maximum(highest_alpha_raw), 3.0)
    lowest_alpha = max(minimum(lowest_alpha_raw), 0.25)

    alpha_lgmean = data[:lgalpha_mean][1]
    alpha_lgsigma = data[:lgalpha_sd][1]
    alpha_bar = data[:alpha_bar_bayes][1]
    alpha_bar_bar = data[:alpha_bar_bar][1]
    alpha_min_bin = data[:alpha_min][1]
    max_num_bidders = data[:max_num_bidders][1]

    # Grab all the alphas
    alphas_curr_contract = filter(x -> x.contract_no == contract_no, alpha_data)
    alphas = alphas_curr_contract.alpha

    alpha_coef = data[:pois_gamma_alphacoef][1]
    auction_fe = data[:pois_gamma_projfe][1]

    proj_manager_id = data[:proj_manager_id]
    engineer_id = data[:engineer_id]
    designer_id = data[:designer_id]
    bin_id = data[:potential_bidder_bin_id]

    # Project-specific info
    project = DataFrame(CSV.File(joinpath(in_dir, "sample_project_$contract_no.csv")))

    q_a = convert(Array{Float64}, project[!, :q_at])
    q_a_model = convert(Array{Float64}, project[!, :q_at_model])
    q_o = convert(Array{Float64}, project[!, :q_ot])
    c = convert(Array{Float64}, project[!, :office_unit_price] / dollar_scale)
    sigma_sq = convert(Array{Float64}, project[!, :sigma_t_sq])
    gamma = hascol(:gamma, project) ? project[!, :gamma][1] : missing
    T = length(q_a)
    item_ids = collect(1:T)

    # alpha_coef = project[:, :pois_gamma_alphacoef][1]
    # auction_fe = project[:, :pois_gamma_projfe][1]

    # Bidder data if available
    pth = joinpath(in_dir, "sample_project_bidders_$contract_no.csv")
    bidders = if isfile(pth)
        bidder_data = DataFrame(CSV.File(pth))

        ## Note: in the estimation data we're saving "skewness" in the bids 
        ##  indexing is 3 - numbidders+2. Prob don't need it for the CF, but 
        ## maybe good for consistency w estimation.
        bidders = [
            Bidder(
                string(names(bidder_data)[i]),
                convert(Array{Float64}, bidder_data[:, i] / dollar_scale),
                (((bidder_data[:, i])' * q_o) / dollar_scale),
            ) for i = 2:(num_bidders+1)
        ]

        bidders = sort!(bidders, by=x -> x.score)
    else
        missing # No bidders, go without.
    end

    return Auction(
        contract_no,
        num_bidders,
        T,
        bidders,
        q_a,
        q_a_model,
        q_o,
        c,
        sigma_sq,
        gamma,
        item_ids,
        extra_work_payment,
        winning_alpha,
        highest_alpha,
        lowest_alpha,
        alpha_lgmean,
        alpha_lgsigma,
        dollar_scale,
        # risky_cost_term_mean,
        # risky_cost_term_var,
        # risky_cost_term_variance_sans_gamma,
        alpha_coef,
        auction_fe,
        proj_manager_id,
        engineer_id,
        designer_id,
        alphas,
        alpha_bar,
        alpha_bar_bar,
        max_num_bidders,
        bin_id,
        alpha_min_bin
    )
end
