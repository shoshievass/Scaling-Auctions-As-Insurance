abstract type AlphaSample end
struct ObservedAlpha <: AlphaSample end
struct RandomAlpha <: AlphaSample
    n::Int
end
struct SpecificAlpha <: AlphaSample
    alphas::Vector{Float64}
end
struct UniformAlpha <: AlphaSample
    n::Int
end

function get_alphas(auc::Auction, config::Config, sampler::RandomAlpha)
    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        auc.lowest_alpha,
        auc.alpha_bar_bar,
    )

    return rand(alpha_dist, sampler.n)
end
get_alphas(auc, config, sampler::SpecificAlpha) = sampler.alphas
get_alphas(auc::Auction, config, sampler::ObservedAlpha) = auc.alphas
get_alphas(auc::Auction, config, sampler::UniformAlpha) = range(auc.alpha_min_bin, auc.alpha_bar_bar, length=sampler.n)

"""
    win_density

"""
function win_density(auc::Auction, config::Config, a)
    # Assemble all the bits
    n_bidders = bidder_count(auc, config)

    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        auc.lowest_alpha,
        auc.alpha_bar_bar,
    )

    F(a) = cdf(alpha_dist, a)

    return (1 - F(a))^(n_bidders - 1)
end

function static_utility(
    auc::Auction,
    config::Config,
    sampler::AlphaSample,
    s_star,
    q
)
    # Sample alphas
    alphas = sort(get_alphas(auc, config, sampler))

    # Calculate winning probability
    win_probs = map(a -> win_density(auc, config, a), alphas)

    # Calculate scores
    gammas = map(a -> get_gamma(auc, config, a), alphas)
    scores = map(s_star, alphas)
    opt_bids = map(x -> optimal_bid(auc, config, x...), zip(scores, alphas)) # (s, alpha)
    ces_actual = map(x -> x.ce, opt_bids)
    ces = entry_cost(auc, config) .+ ces_actual # Add entry cost back in

    # return the values
    return (
        contract_no=auc.contract_no,
        tag=config.tag,
        K=config.entry.amount,
        n=config.entry.n_entrants,
        λ=config.ewo.amount,
        pred_entry=mean(ces_actual .>= 0),
        q=q,
    )
end
