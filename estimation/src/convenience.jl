"""
    inflate(auc::Auction, config::Config, z, y_bar)

Inflate used quantities, given a vector of fudge quantities `y_bar` and
a vector `z` of 1/0s indicating whether to fudge item `t`.
"""
inflate(auc::Auction, config::Config, opt::OptimalBid) = inflate(auc, config, opt.shade)
inflate(auc::Auction, config::Config, ::Nothing) = estimated(auc, config)
function inflate(auc::Auction, config::Config, shade)
    y_bar = get_ybar(auc, config)
    q = estimated(auc, config)
    q_mod = q .* (1 .+ shade .* (y_bar))
    return q_mod
end

function score_range(auc::Auction)
    scores = sort(map(z -> z.score, auc.bidders), rev=true)
    return scores[1], scores[end] # highest, lowest
end

function alpha_max(auc::Auction, config::Config)
    # return auc.highest_alpha - ALPHA_EPSILON
    return auc.alpha_bar - ALPHA_EPSILON
end

function alpha_min(auc::Auction, config::Config)
    return 0.5
    # return auc.lowest_alpha + ALPHA_EPSILON
end

function alpha_span(auc::Auction, config::Config)
    return alpha_max(auc, config), alpha_min(auc, config)
end

get_gamma(auc::Auction, config::Config, alpha=nothing) = get_gamma(auc, config.risk_aversion, alpha)
get_gamma(auc::Auction, ra::HomogeneousRisk, alpha) = ra.gamma
function get_gamma(auc::Auction, ra::HeterogeneousRisk, alpha)
    return exp(ra.coef * log(alpha) + ra.fe)
end
get_gamma(auc::Auction, ra::HeterogeneousRisk, alpha::Nothing) =
    throw(ArgumentError("Requires non-missing alpha"))

get_sigmasq(auc::Auction, config::Config) = get_sigmasq(auc, config.risk_adjust)

"""
    get_sigmasq(auc::Auction, rs::RiskScaling)
"""
get_sigmasq(auc::Auction, rs::RiskScaling) = auc.sigma_t_sq .* rs.delta
function get_sigmasq(
    auc::Auction,
    config::Config{R,A,Rt,H}
) where {R<:RiskAversion,A<:Lump,Rt<:RiskScaling,H<:MoralHazard}
    # Lump + moral hazard means set sigma=0 for
    # all t that are hazardable.
    t_basic = auc.sigma_t_sq .* config.risk_adjust.delta
    for t in 1:auc.T
        if config.hazard.y_bar[t] > 0
            t_basic[t] = 0
        end
    end
    
    return t_basic
end

"""
    get_sigmasq(auc::Auction, ::SymmetricRisk)

Set all risk to the same number. Average risk is set to
the mean risk, weighted by q_o * cost / score.
"""
function get_sigmasq(auc::Auction, ::SymmetricRisk)
    # Get estimated risk
    risk = auc.sigma_t_sq

    # Calculate weights
    expense = auc.c .* auc.q_o

    # Get engineering score
    s = sum(expense)

    # Weight the risk
    weighted = sum(risk .* expense ./ s)

    return ones(size(risk)) .* weighted
end

get_ybar(auc::Auction, config::Config) = get_ybar(auc, config.hazard)
get_ybar(auc::Auction, hz::NoHazard) = zeros(auc.T)
get_ybar(auc::Auction, hz::MoralHazard) = hz.y_bar

# Retrieves q_a, q_a_model, or q_o depending on configuration.
estimated(auc::Auction, config::Config) = estimated(auc, config.quantity)
estimated(auc::Auction, qtype::ActualPrediction) = auc.q_a
estimated(auc::Auction, qtype::EngineerPrediction) = auc.q_o
estimated(auc::Auction, qtype::ModelPrediction) = auc.q_a_model
function estimated(auc::Auction, config::Config{R,A,Rt,H}) where {R<:RiskAversion,A<:Lump,Rt<:RiskType,H<:MoralHazard}
    base = estimated(auc, config.quantity)
    return (1 .- config.hazard.y_bar) .* base
end

bidder_count(auc::Auction, config::Config) = config.entry.n_entrants
entry_cost(auc::Auction, config::Config) = config.entry.amount

# Bid lower bounds
bid_lower(auc::Auction, config::Config) = auc.c .* config.bid_limit.fraction

# Calculated arithmatically
function get_risky_mean(auc::Auction, config::Config)
    return auc.c'estimated(auc, config)
end

function get_risky_cost_var(auc::Auction, config::Config)
    return sum(auc.c .^ 2 .* get_sigmasq(auc, config)) / 2
end

function get_risky_cost_var_gamma(auc::Auction, config::Config, alpha)
    return get_gamma(auc, config, alpha) * get_risky_cost_var(auc, config)
end

function office_score(auc::Auction)
    return auc.c'auc.q_o
end

# monopoly_rent(thing::FixedAmount, os) = os + thing.amount
monopoly_rent(thing::ScaledMonopoly, os) = os * thing.multiple
function monopoly_rent(auc::Auction, config::Config)
    # Calculate office score
    os = office_score(auc)

    # Inflate the office score
    return monopoly_rent(config.monopoly, os)
end

entry_utility(bid::Missing, args...) = missing
function entry_utility(bid::OptimalBid, gamma, K)
    return 1 - exp(-gamma * (bid.ce - K))
end
