function certainty_equivalent(auc, config, alpha, s)
    return certainty_equivalent(auc, config, alpha, s, nothing, nothing)
end

function certainty_equivalent(
    auc::Auction,
    config::Config{R,A},
    alpha,
    s,
    ::Nothing,
    ::Nothing
) where {R,A<:Lump}
    # Calculate intermediate values
    risk_mean = get_risky_mean(auc, config)
    risk_var = get_risky_cost_var_gamma(auc, config, alpha)

    # Handle EWO
    ewo_payoff = (config.ewo.amount * auc.extra_work_payment)

    # Calculate certainty equivalent
    ce = s - (alpha * risk_mean + alpha^2 * risk_var) + ewo_payoff

    # Return it!
    return ce
end

function certainty_equivalent(
    auc::Auction,
    config::Config{R,A},
    alpha,
    s,
    b,
    z;
    verbose=false
) where {R,A<:Union{Scaling, Mixed}}
    # Calculate intermediate values
    gamma = get_gamma(auc, config, alpha)
    λ = config.auctype isa Mixed ? config.auctype.lambda : 1.0
    q = inflate(auc, config, z) # Accounts for moral hazard
    q_e = auc.q_o
    c = auc.c
    sigma_sq = get_sigmasq(auc, config)

    # Handle misc payoffs
    ewo_payoff = config.ewo.amount * auc.extra_work_payment

    payoff = (λ * q + (1 - λ) * q_e)' * b - alpha * q'c - gamma / 2 * sigma_sq' * ((λ * b - alpha * c) .^ 2)

    ce = payoff + ewo_payoff

    return ce
end
