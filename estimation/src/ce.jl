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
    # gamma = get_gamma(auc, config, alpha)
    # q = estimated(auc, config)
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

# function certainty_equivalent(
#     auc::Auction,
#     config::Config{R,A},
#     alpha,
#     s,
#     b,
#     z;
#     verbose=false
# ) where {R,A<:Mixed}
#     # Calculate intermediate values
#     gamma = get_gamma(auc, config, alpha)
#     λ = config.auctype.lambda
#     q = inflate(auc, config, z) # Accounts for moral hazard
#     q_e = auc.q_o
#     c = auc.c
#     sigma_sq = get_sigmasq(auc, config)

#     # Handle misc payoffs
#     ewo_payoff = config.ewo.amount * auc.extra_work_payment

#     mixed_q = (λ * q + (1 - λ) * q_e)
#     payoff = mixed_q' * b - alpha * q'c - gamma / 2 * sigma_sq' * ((λ * b - alpha * c) .^ 2)

#     # calculate CE
#     ce = payoff + ewo_payoff
#     # @info "CE" ce b q'b

#     verbose && display([b q q_e sigma_sq c])
#     verbose && @info "CE" payoff ewo_payoff ce entry gamma s mixed_q' * b alpha * q'c gamma / 2 * sigma_sq' * (λ * b - alpha * c) .^ 2 alpha config.entry.n_entrants
#     # @info "ce" payoff ewo_payoff entry q'b q_e'b alpha*q'c gamma (gamma/2 * sigma_sq' * (λ*b - alpha*c) .^ 2) ce

#     # if λ == 0
#     #     risk_mean = get_risky_mean(auc, config)
#     #     risk_var = get_risky_cost_var_gamma(auc, config, alpha)

#     #     # Calculate certainty equivalent
#     #     lump_ce = s - (alpha * risk_mean + alpha^2 * risk_var) + ewo_payoff

#     #     @info "" ForwardDiff.value(s) ForwardDiff.value(lump_ce) ForwardDiff.value(ce)
#     # end

#     # lambda_b_min_c = (λ*b) - (alpha*c);
# 	# mixed_q = (λ*q + (1-λ)*q_e)

#     # risk_mean = get_risky_mean(auc, config)
#     # risk_var = get_risky_cost_var_gamma(auc, config, alpha)
#     # cost_term = alpha*risk_mean + alpha^2 * gamma * risk_var
		
# 	# profit_ce = (mixed_q' * b -  alpha * (q' * c)) - (0.5 * gamma) * ((sigma_sq .* lambda_b_min_c)' * lambda_b_min_c)

#     # @info "ce-mixed" ForwardDiff.value(payoff) ForwardDiff.value(profit_ce) ForwardDiff.value(ewo_payoff) ForwardDiff.value(cost_term) λ ForwardDiff.value(s)

#     return ce
# end

