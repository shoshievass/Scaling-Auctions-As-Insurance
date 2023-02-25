# using Pkg; Pkg.activate(@__DIR__); Pkg.instantiate()
# using Revise
using Counterfactual
using Dates
using DataFrames, CSV
using Statistics
using ProgressMeter
using Plots
using LinearAlgebra
using Polynomials
using BlackBoxOptim
using Optim
using StatsBase
using Distributions
using Random
using ThreadsX
import UnicodePlots

println("Threadcount $(Threads.nthreads())")

function change_monopolist(auc, λ, solutions, scaler, k)
    # Config
    config = configurate(auc, λ, 1; monopoly_scaler=scaler, entry_cost=k)

    # estimate n=1 eq
    eq, sol = equilibrium(auc, config; silent=false, verbose=false)

    # Return a similar array
    newsols = deepcopy(solutions)
    newsols[1] = sol
    return newsols
end

function configurate(auc, λ, n; monopoly_scaler=1.25, entry_cost=0)
    # Set K to zero always
    return Config(
        auc;
        solver=ExactSolver(),
        tag="entry-λ-$λ-n-$n-monopoly-$(monopoly_scaler)",
        entry=EntryParams(entry_cost, n),
        ewo=ExtraWorkOrder(λ),
        monopoly=ScaledMonopoly(monopoly_scaler)
    )
end

function polyexp(x, coefs)
    Y = [x^(i-1) * coefs[i] for i in 1:length(coefs)]
    return exp(sum(Y))
end

function polyerror(xs, ys, coefs)
    #
    ys_hat = map(x -> polyexp(x, coefs), xs)
    err = sum((ys_hat - ys) .^ 2)
    return err
end

function polyfit(xs, ys_unscaled, order=missing)
    if length(collect(skipmissing(ys_unscaled))) == 0
        return ys_unscaled, ys_unscaled, length(ys_unscaled)
    end

    # Get the max to scale by
    ymax = maximum(skipmissing(ys_unscaled)) * 1
    ys = ymax > 0 ? ys_unscaled / ymax : ys_unscaled

    # Enforce monotonicity
    prev_loc = 2
    for i in 3:length(ys)
        val = ys[i]
        if ismissing(val)
            # Try linear interpolation first
            # Find next non-missing value
            for j in (i+1):length(ys)
                next_val = ys[j]
                # @info "interpolating" i ys[i] ys[prev_loc] next_val length(ys) j

                if !ismissing(next_val) && !ismissing(ys[prev_loc]) && next_val < ys[prev_loc]
                    grad = (ys[prev_loc] - next_val) / (j - i)
                    ys[i] = (ys[prev_loc] + next_val) / 2
                    # @info "" i j ys[i] ys[prev_loc] next_val
                    interpolated = true
                    break
                end
            end
        end

        prev_loc = !ismissing(val) ? i : prev_loc
    end

    for i in 3:length(ys)
        if ismissing(ys[i])
            ys[i] = ys[i-1]
        end
    end


    # Filtered ys
    inds = map(i -> !ismissing(ys[i]) && !isnan(ys[i]) && ys[i] > 0, eachindex(ys))

    if sum(inds) <= 1 # No data!
        return ys_unscaled, ys_unscaled, length(ys_unscaled)
    elseif sum(inds) == length(inds) # Have everything
        return ys_unscaled, ys .* ymax, 0
    end

    ys_filtered = map(identity, ys[inds])
    xs_filtered = xs[inds]

    target(params) = polyerror(xs_filtered, ys_filtered, params)

    # calculate polyfit for remainder
    order = ismissing(order) ? sum(inds) : min(order, sum(inds))
    result = optimize(target, zeros(order))
    y_hats = map(i -> ismissing(ys[i]) || ys[i] < 0 ? polyexp(xs[i], result.minimizer) : ys[i], eachindex(xs))

    return ys .* ymax, y_hats .* ymax, length(inds) - sum(inds)
end

function polyinterp(x, y)
    # call polyfit functions
    actual_y, fitted_y, num_interpolated = polyfit(x, y, 4)
    return fitted_y, num_interpolated

end

function solve_alpha_bar(auc::Auction, config::Config; use_cdf=true)
    # Extractions
    k = config.entry.amount
    M = auc.max_num_bidders
    q = auc.num_bidders / M

    # Precalculate binomial coefficients
    θs = [binomial(M - 1, m - 1) * q^(m - 1) * (1 - q)^(M - m) for m in 2:(M)]

    # Config
    single_entrant_config = entrants(config, 1)

    # Define optimization target
    function opt_target(a; verbose=false)
        try
            # Make a new auction with the provided alpha bar
            new_auction = deepcopy(auc)
            new_auction.alpha_bar = a

            ##############
            # Entry norm #
            ##############
            alpha_dist = truncated(
                LogNormal(new_auction.alpha_lgmean, new_auction.alpha_lgsigma),
                new_auction.lowest_alpha,
                new_auction.alpha_bar_bar,
            )

            acdf = cdf(alpha_dist, a)

            # Calculate bid function for n=1
            _, sol = equilibrium(new_auction, single_entrant_config; silent=false, verbose=false)

            γ = Counterfactual.get_gamma(new_auction, single_entrant_config, a)
            s = sol(a)
            opt = Counterfactual.optimal_bid(new_auction, single_entrant_config, s, a)

            sum_bit = sum(θs .* (1 - exp(γ * k)))
            winner = (1 - q)^(M - 1) * (1 - exp(-γ * (opt.ce - k)))

            if use_cdf
               
                prob_monopoly = (1 - q)^(M - 1)
                prob_competition = 1 - prob_monopoly

                entry_eu = exp(-γ * k)
                monopoly_eu = prob_competition + prob_monopoly.*(exp(-γ * opt.ce))
                monopoly_total_eu = prob_competition.*(1 - exp(γ * k)) + prob_monopoly.*(1 - exp(-γ * (opt.ce - k)))

                l1norm = (entry_eu - monopoly_eu)^2

                return l1norm, sol, monopoly_total_eu, opt
            else
               
                prob_monopoly = (1 - acdf)^(M - 1)
                prob_competition = 1 - prob_monopoly

                entry_eu = exp(-γ * k)
                monopoly_eu = prob_competition + prob_monopoly.*(exp(-γ * opt.ce))
                monopoly_total_eu = prob_competition.*(1 - exp(γ * k)) + prob_monopoly.*(1 - exp(-γ * (opt.ce - k)))

                l1norm = (entry_eu - monopoly_eu)^2
                return l1norm, sol, monopoly_total_eu, opt
            end
            
        catch e
            if e isa InterruptException
                rethrow(e)
            end

            println("solve_alpha_bar error")
            println(typeof(e))
            return Inf, x -> missing, missing, missing
        end
    end

    res = optimize(z -> opt_target(z)[1], auc.alpha_min_bin, auc.alpha_bar_bar, Brent())

    _,_,ce,opt = opt_target(res.minimizer)

    if Optim.converged(res)

        return res.minimizer, res.minimum, ce, opt
    else
        return missing, missing, missing, opt
    end
end

function solve_alpha_bar_and_k(auc::Auction, config::Config)
    # Extractions
    M = auc.max_num_bidders
    q = auc.num_bidders / M

    # Precalculate binomial coefficients
    θs = [binomial(M - 1, m - 1) * q^(m - 1) * (1 - q)^(M - m) for m in 2:(M)]

    # Config
    single_entrant_config = entrants(config, 1)

    # Define optimization target
    function opt_target(θ; verbose=false)
        a, k = θ
        try
            # Make a new auction with the provided alpha bar
            new_auction = deepcopy(auc)
            new_auction.alpha_bar = a

            ##############
            # Entry norm #
            ##############
            alpha_dist = truncated(
                LogNormal(new_auction.alpha_lgmean, new_auction.alpha_lgsigma),
                new_auction.lowest_alpha,
                new_auction.alpha_bar_bar,
            )

            acdf = cdf(alpha_dist, a)

            # Calculate bid function for n=1
            _, sol = equilibrium(new_auction, single_entrant_config; silent=false, verbose=false)

            γ = Counterfactual.get_gamma(new_auction, single_entrant_config, a)
            s = sol(a)
            opt = Counterfactual.optimal_bid(new_auction, single_entrant_config, s, a)

            sum_bit = sum(θs .* (1 - exp(γ * k)))
            winner = (1 - q)^(M - 1) * (1 - exp(-γ * (opt.ce - k)))

            l1norm = (sum_bit + winner)^2 + (acdf- q) ^ 2
            return l1norm, sol, winner + sum_bit
            
        catch e
            rethrow(e)
            if e isa InterruptException
                rethrow(e)
            end

            println("solve_alpha_bar error")
            println(typeof(e))
            return Inf, x -> missing, missing
        end
    end

    lb = [auc.alpha_min_bin, 0.0]
    ub = [auc.alpha_bar_bar, Inf]
    res = optimize(z -> opt_target(z)[1], lb, ub, [auc.alpha_bar, config.entry.amount])
    display(res)
    a,k = res.minimizer
    @info "" a k
    _,_,ce = opt_target(res.minimizer)


    if Optim.converged(res)
        
        return res.minimizer, res.minimum, ce#, opt.s, any_monopolist
    else
        return missing, missing, missing
    end
end

function solve_k(auc::Auction, config::Config, λ, M, q, max_scalar)
    a = auc.alpha_bar

    g_grid = [max_scalar]
    bids = zeros(length(auc.c), length(g_grid))
    ces = zeros(1, length(g_grid))
    ks = zeros(1, length(g_grid))
    utils = zeros(1, length(g_grid))
    scores = zeros(1, length(g_grid))
    diffs = zeros(1, length(g_grid))
    θs = [binomial(M - 1, m - 1) * q^(m - 1) * (1 - q)^(M - m) for m in 2:(M)]

    for (i, scaler) in enumerate(g_grid)
        # Config
        config = configurate(auc, λ, 1; monopoly_scaler=scaler)

        try
            _, sol = equilibrium(auc, config; silent=false, verbose=false)

            γ = Counterfactual.get_gamma(auc, config, a)
            opt = Counterfactual.optimal_bid(auc, config, sol(a), a)
            bids[:, i] = opt.bid
            ce = opt.ce
            ces[1, i] = ce
            utils[1, i] = opt.utility
            scores[1, i] = opt.s


            function opt_target(k; verbose=false)
                sum_bit = sum(θs .* (1 - exp(γ * k)))
                winner = (1 - q)^(M - 1) * (1 - exp(-γ * (ce - k)))

                l1norm = (sum_bit + winner)^2
                return l1norm
            end

            res = optimize(opt_target, 0.0, 100.0, GoldenSection())
            ks[1, i] = res.minimizer
            diffs[1, i] = res.minimum

            if i > 1 && ces[1, i-1] > ce
                # deprecated error checking 
            end
        catch e
            return ces[1, 1:(i-1)],
            ks[1, 1:(i-1)],
            g_grid[1:(i-1)],
            scores[1, 1:(i-1)],
            utils[1, 1:(i-1)],
            diffs[1, 1:(i-1)]
        end
    end

    return ces[1, :],
    ks[1, :],
    g_grid[:],
    scores[1, :],
    utils[1, :],
    diffs[1, :]
end

function exante_alpha(auc, λ, alpha, sols, M, q, k)
    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        auc.lowest_alpha,
        auc.alpha_bar_bar,
    )

    F(a) = cdf(alpha_dist, a)

    θs = [binomial(M - 1, m - 1) * q^(m - 1) * (1 - q)^(M - m) for m in 1:M]
    configs = map(n -> configurate(auc, λ, n), 1:M)
    scores = map(s -> s(alpha), sols)

    # Winning scenario
    bids = map(
        x -> ismissing(x[2]) ? missing : Counterfactual.optimal_bid(auc, x[1], x[2], alpha),
        zip(configs, scores)
    )

    # Grab the gamma for this alpha
    gamma = Counterfactual.get_gamma(auc, configs[1], alpha)

    ew = map(x -> entry_utility(x, gamma, k), bids)
    ew, n_interpolated = polyinterp(collect(1:M), ew)

    if any(ismissing.(ew))
        return missing, n_interpolated
    else
        pw = map(m -> (1 - F(alpha))^(m - 1), 1:M)

        # Losing scenario
        el = repeat([1 - exp(-gamma * (-k))], length(ew))
        pl = map(m -> 1 - (1 - F(alpha))^(m - 1), 1:M)

        @assert all(pw + pl .== 1) "Probabilities do not sum to one"

        # Return the ex-ante expected utility
        return dot(θs, ew .* pw + el .* pl), n_interpolated
    end
end

function finite_only(xs)
    return filter(isfinite, xs)
end

function moments(auctions, λ, krange; alpha_samples=100)
    # Iterate through auctions
    moment_vec = []

    # Add an integer index to the auctioon maps
    println("aucmapped")
    aucmapped = shuffle([(aucno, auc) for (aucno, auc) in enumerate(auctions)])
    datatups = []
    _kbounds = []
    _lock = Threads.ReentrantLock()

    # Threads.@threads for (aucno, auc) in aucmapped
    for (aucno, auc) in aucmapped
        # Check to see if this has been solved already
        path = get(ENV, "GROUP_SCRATCH", "/home/cameron/research/shosh-jmp/tidy/")
        path = joinpath(path, "entry-results")
        ode_path = joinpath(path, "odes")
        k_path = joinpath(path, "cost-grid")
        
        !isdir(path) && mkpath(path)
        !isdir(k_path) && mkpath(k_path)
        path = joinpath(path, "contract-$(auc.contract_no)-lambda-$(λ).csv")
        k_path = joinpath(k_path, "contract-$(auc.contract_no)-lambda-$(λ).csv")

        if false# ispath(path)
            # Skip this one
            println("Skipping contract $(auc.contract_no), lambda $λ")
        else
            # grab entry probability
            auc_N = auc.max_num_bidders
            entry_prob = auc.num_bidders / auc_N

            eus = zeros(Union{Missing,Float64}, auc_N)
            solutions = []
            _inner_lock = Threads.ReentrantLock()

            Threads.@threads for n in 1:auc_N
                
                config = configurate(auc, λ, n)

                # Calculate equilibrium
                !isdir(ode_path) && mkpath(ode_path)
                eq, sol = equilibrium(auc, config; silent=false, verbose=false, solution_folder=ode_path)
                eus[n] = eq.expected_utility

                if eq.retcode == :Success
                    
                    lock(_inner_lock) do
                        push!(solutions, sol)
                    end
                else
                    lock(_inner_lock) do
                        push!(solutions, x -> missing)
                    end
                end
            end

            # Calculate ex-ante alpha
            config = configurate(auc, λ, auc_N)


            alpha_tups = []
            _alpha_tups_lock = Threads.ReentrantLock()
            
            monopolist_grid = [1.25]

            alpha_dist = truncated(
                LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
                auc.lowest_alpha,
                auc.alpha_bar_bar,
            )

            F(a) = cdf(alpha_dist, a)
            alpha_bar_cdf = F(auc.alpha_bar)

            for m in monopolist_grid
                k_ce, k_k, k_g, k_score, k_util, k_diffs = solve_k(auc, config, λ, auc_N, entry_prob, m)
                _, k_loc = findmax(k_ce)
                best_ce, best_k, best_g, best_score = k_ce[k_loc], k_k[k_loc], k_g[k_loc], k_score[k_loc]

                k_df = DataFrame(
                    contract_no=auc.contract_no,
                    lambda=λ,
                    max_scalar=m,
                    ce=k_ce,
                    entry_cost=k_k,
                    monopoly_scalar=k_g,
                    utility=k_util,
                    score=k_score,
                    objective_function=k_diffs,
                    alpha_bar=auc.alpha_bar,
                )
                CSV.write(k_path, k_df)
                println(k_df)

                # Draw some uniform alphas
                newsols = change_monopolist(auc, λ, solutions, m)
                uniform_exante = mean(map(
                    a -> exante_alpha(auc, λ, a, newsols, auc_N, entry_prob, krange)[1] .>= 0,
                    get_alphas(auc, config, UniformAlpha(100))
                ))

                Threads.@threads for s in 1:alpha_samples
                    # Draw a sample of alphas
                    arange = get_alphas(auc, config, RandomAlpha(auc_N))

                    tbuf = []

                    exante = map(
                        a -> a => exante_alpha(auc, λ, a, newsols, auc_N, entry_prob, krange),
                        arange
                    )

                    alpha_bar_exante, _ = exante_alpha(
                        auc,
                        λ,
                        auc.alpha_bar,
                        newsols,
                        auc_N,
                        entry_prob,
                        krange
                    )

                    for a_ind in eachindex(exante)
                        (a, (utilities, interpolated)) = exante[a_ind]

                        for (i, k) in enumerate(krange)
                            nt = (
                                aucno=aucno,
                                contract_no=auc.contract_no,
                                alpha=a,
                                utility=utilities[i],
                                empirical_entry=entry_prob,
                                uniform_entry=uniform_exante[i],
                                alpha_bar_exante=alpha_bar_exante[i],
                                k=k,
                                lambda=λ,
                                monopoly_scalar=m,
                                n_interpolated=interpolated[i],
                                auc_n=auc_N,
                                ewo=auc.extra_work_payment,
                                alpha_sample=s,
                                alpha_bar_cdf=alpha_bar_cdf,
                                best_k=best_k,
                                best_ce=best_ce,
                                best_g=best_g,
                                best_monopoly_rent=best_score,
                                monopoly_rent=Counterfactual.monopoly_rent(auc, configurate(auc, λ, 1; monopoly_scaler=m)),
                                office_score=Counterfactual.office_score(auc),
                            )
                            push!(tbuf, nt)
                        end
                    end

                    lock(_alpha_tups_lock) do
                        append!(alpha_tups, tbuf)
                    end
                end
            end

            alpha_df = DataFrame(alpha_tups)

            entry_guess = combine(
                groupby(alpha_df, [:contract_no, :k, :lambda, :monopoly_scalar]),
                :utility => mean ∘ finite_only => :mean_utility,
                :utility => median ∘ finite_only => :median_utility,
                :utility => std ∘ finite_only => :std_utility,
                :alpha_bar_exante => first => :alpha_bar_exante,
                :empirical_entry => mean ∘ finite_only => :empirical_entry,
                :uniform_entry => mean ∘ finite_only => :uniform_entry,
                :utility => (u -> mean(u .>= 0)) => :theoretical_entry,
                :n_interpolated => mean => :n_interpolated_avg,
                :n_interpolated => sum => :n_interpolated_total,
                :ewo => first => :ewo,
                :auc_n => first => :auc_n,
                :alpha => minimum ∘ finite_only,
                :alpha => maximum ∘ finite_only,
                :alpha_bar_cdf => first => :alpha_bar_cdf,
                :best_k => first => :best_k,
                :best_ce => first => :best_ce,
                :best_g => first => :best_g,
                :best_monopoly_rent => first => :best_monopoly_rent,
                :office_score => first => :office_score,
                :monopoly_rent => first => :monopoly_rent,
            )

            lock(_lock) do
                if !ispath(path)
                    CSV.write(path, entry_guess)
                end
            end
        end
    end

    println("λ $(λ) complete")
    return
end

function partition_jobs(things, total_procs)
    target_size = max(1, div(length(things), total_procs))
    retval = Vector{Vector{eltype(things)}}(undef, total_procs)

    for i in 1:total_procs
        start_ind = (i - 1) * target_size + 1
        end_ind = min(i * target_size, length(things))
        retval[i] = things[start_ind:end_ind]
    end

    return retval
end
