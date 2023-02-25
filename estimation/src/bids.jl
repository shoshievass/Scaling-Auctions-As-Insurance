struct OptimalBid{A,S,B,Y,C,U,CO,CT}
    alpha::A
    s::S
    bid::B
    shade::Y
    ce::C
    utility::U
    cost::CO
    compute_time::CT
end

function OptimalBid(
    auc::Auction,
    config::Config,
    alpha,
    s,
    bid,
    shade,
    cost,
    compute_time
)
    # Calculate CE & utility
    gamma = get_gamma(auc, config, alpha)
    ce = certainty_equivalent(auc, config, alpha, s, bid, shade)
    utility = 1 - exp(-gamma * ce)

    # Store optimal bid
    ob = OptimalBid(alpha, s, bid, shade, ce, utility, value(cost), compute_time)
    return ob
end

function gurobi_retry()
    @suppress begin
        while true
            try
                m = Model(() -> Gurobi.Optimizer(GRB_ENV[]))
                set_optimizer_attribute(m, "OutputFlag", 0)
                set_optimizer_attribute(m, "Method", 3)
                # m = Model(Gurobi.Optimizer)
                set_optimizer_attribute(m, "Threads", 12) # For the yens
                return m
            catch e
                println("Gurobi license failing")
                println(e)
                sleep(3)
                rethrow(e)
            end
        end
    end
end

function construct_optimizer(auc::Auction, config::Config{R,A,Rt,H,E,S,Q,En}, s, alpha) where {R,A<:Union{Scaling,Mixed},Rt,H,E,S<:NumericSolver,Q,En}
    # initalize optimizer

    # println("\t s: $(s) alpha: $(alpha) ")
    m = if !GUROBI
        if config.hazard isa MoralHazard
            Model(SCIP.Optimizer)
        else
            Model(Ipopt.Optimizer)
        end
    else
        # Use Gurobi
        @suppress begin
            gurobi_retry()
        end
    end

    # set_optimizer_attribute(m, "MIPFocus", 3)
    # If you are more interested in good quality feasible solutions, you can select MIPFocus=1.
    # If you believe the solver is having no trouble finding the optimal solution, and wish to focus more
    # attention on proving optimality, select MIPFocus=2.
    # If the best objective bound is moving very slowly (or not at all), you may want to try MIPFocus=3 to focus on the bound.

    # Silence optimizer output
    JuMP.set_silent(m)

    # Add solver time limit
    @suppress begin
        JuMP.set_time_limit_sec(m, 1000.0)
    end

    # Declare variables
    y_bar = get_ybar(auc, config)
    z = zeros(Int8, auc.T)

    # Check whether to add moral hazard info
    bid_lb = bid_lower(auc, config) # Zero if no min bid, auc.c * frac otherwise.

    if config.hazard isa MoralHazard
        @variables m begin
            b[i=1:auc.T] >= 0
            z[i=1:auc.T], Bin
        end
    else
        # No moral hazard permitted, only optimize bids.
        @variables m begin
            b[i=1:auc.T] >= 0
        end
    end

    # Lower bound everything
    set_lower_bound.(b, bid_lb)

    # Extract auction-specific values for risk aversion & unit variance.
    gamma = get_gamma(auc, config, alpha)
    sigma_sq = get_sigmasq(auc, config)
    q_forecast = estimated(auc, config)
    q_engineer = auc.q_o

    # Generate objective function
    risk = gamma / 2

    if config.auctype isa Scaling
        # Use scaling objective
        @objective(m, Max, sum((1 + y_bar[i] * z[i]) * q_forecast[i] * (b[i] - alpha * auc.c[i]) -
                               risk * sigma_sq[i] * (b[i] - alpha * auc.c[i])^2 for
                               i = 1:auc.T))
    elseif config.auctype isa Mixed
        # Extract lambda
        lambda = config.auctype.lambda

        # Renegotiation
        @objective(m, Max, sum(b[i] * q_engineer[i] +
                               (lambda * b[i] * ((1 + y_bar[i] * z[i]) * q_forecast[i] - q_engineer[i])) -
                               alpha * auc.c[i] * (1 + y_bar[i] * z[i]) * q_forecast[i]
                               -
                               risk * sigma_sq[i] * (lambda * b[i] - alpha * auc.c[i])^2 for i in
                               1:auc.T))

    end

    # Add constraints
    @constraint(m, sum(b[i] * q_engineer[i] for i in 1:auc.T) == s)

    return m, b, z
end

function optimal_bid(auc::Auction, config::Config{R,A,Rt,H,E,S,Q,En}, s,
    alpha,) where {R,A<:Union{Scaling,Mixed},Rt,H,E,S<:NumericSolver,Q,En}
    # Start a timer.
    start_time = now()

    # Make the optimizer
    m, b, z = construct_optimizer(auc, config, s, alpha)

    # Optimize it, homie
    optimize!(m)
    # termination_status(m) |> println

    # Extract resulting values
    b_min = map(value, value.(b))
    z_min = map(value, value.(z))

    # Calculate some numbers
    cost = b_min'auc.q_a

    # Stop the timer.
    compute_time = now() - start_time

    # Return the STUFF, we did it.
    return OptimalBid(auc, config, alpha, s, b_min, z_min, cost, compute_time)
end

function optimal_bid(auc::Auction, config::Config{R,A,Rt,H,E,S,Q,En}, s,
    alpha,) where {R,A<:Lump,Rt,H,E,S,Q,En}
    # Start a timer.
    start_time = now()

    # Stop the timer.
    compute_time = now() - start_time

    # Return the STUFF, we did it.
    return OptimalBid(auc, config, alpha, s, nothing, nothing, s, compute_time)
end

function db_ds_deriv(::NumericSolver, target, s)
    return FiniteDiff.finite_difference_derivative(target, s)
end

function db_ds_deriv(::ExactSolver, target, s)
    return ForwardDiff.derivative(target, s)
end

function db_ds(auc::Auction, config::Config{R,A,Rt,H,E,S,Q,En}, s, alpha,) where {R,A,Rt,H,E,S,Q,En}
    # Define a function to take the derivative of.
    function target(x)
        return optimal_bid(auc, config, x, alpha).bid
    end

    # Calculate it
    derivative = db_ds_deriv(config.solver, target, s)

    # Send it back
    return derivative
end

function calc_boundary(
    auc::Auction,
    config::Config{R,A,Rt,H,E,S,Q,En},
) where {R,A<:Union{Scaling,Mixed,Lump},Rt,H,E,S,Q,En}
    # Retrieve the maximum alpha
    am = alpha_max(auc, config)

    # Get the range of scores seen.
    highest_s, lowest_s = score_range(auc::Auction)

    # Create the function to zero
    function target(x)
        try
            ob = optimal_bid(auc, config, x[1], am)
            

            return ob.ce^2
        catch e
            rethrow(e)
            println(typeof(e))
            if e isa InterruptException || (e isa ErrorException) || e isa AssertionError || e isa TypeError || e isa UndefRefError
                rethrow(e)
            end

            if e isa MethodError || e isa UndefVarError
                rethrow(e)
            end

            println("\t Error: $e")

            
            return lowest_s^2
        end
    end

    # Solve it (or die trying)
    nl_solution = nlsolve(target, [lowest_s])

    try
        s = minimum(nl_solution.zero)
        fitness = target(s)
        # println("Fitness: ", fitness)
        @assert fitness <= 1e-2

        if isnan(s)
            error("NaN boundary")
        else
            return minimum(nl_solution.zero)
        end
    catch e
        println("!!! Retrying boundary solver")
        # Try to use bboptim instead
        solution = bboptimize(
            target,
            [0.0],
            SearchRange=(0, highest_s * 50),
            NumDimensions=1,
            TargetFitness=0.0,
            TraceMode=:silent,
            # MaxTime=30,
            Method=:generating_set_search
        )

        if best_fitness(solution) <= 1e-5
            # We have a candidate, give that instead.
            return only(best_candidate(solution))
        end

        rethrow(e)
    end
end


function ds_dc(
    auc::Auction,
    config::Config{R,A,Rt,H,E,S,Q,En},
    s,
    alpha,
) where {R,A<:Scaling,Rt,H,E,S,Q,En}
    # Prerequisites
    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        auc.lowest_alpha,
        auc.alpha_bar,
    )

    f(a) = pdf(alpha_dist, a)
    F(a) = cdf(alpha_dist, a)

    # Calculate optimal bids
    opt = optimal_bid(auc, config, s, alpha)

    # Inflate if relevant (does nothing if no moral hazard)
    q = inflate(auc, config, opt)

    # Calculate db/ds
    dbds = db_ds(auc, config, s, alpha)

    # Assemble all the bits
    gamma = get_gamma(auc, config, alpha)
    n_bidders = bidder_count(auc, config)
    sigma_sq = get_sigmasq(auc, config)

    b_min_c = opt.bid - (alpha .* auc.c)

    profit_term = exp(gamma * opt.ce) - 1
    numerator = f(alpha) ./ (1 - F(alpha) - 1e-6) * (n_bidders - 1) * profit_term
    denominator = dbds' * ((gamma .* q) - (gamma^2 .* sigma_sq .* b_min_c))

    # Calculate derivative
    ds_dc = numerator / denominator

    # Toss it back to the caller
    return ds_dc
end


function ds_dc(
    auc::Auction,
    config::Config{R,A,Rt,H,E,S,Q,En},
    s,
    alpha
) where {R,A<:Lump,Rt,H,E,S,Q,En}
    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        # 0.5,
        auc.lowest_alpha,
        auc.alpha_bar
    )

    f(a) = pdf(alpha_dist, a)
    F(a) = cdf(alpha_dist, a)

    gamma = get_gamma(auc, config, alpha)
    risk_mean = get_risky_mean(auc, config)
    risk_var = get_risky_cost_var(auc, config)

    ce = certainty_equivalent(auc, config, alpha, s)
    exp_profit = exp(gamma * ce)
    profit_term = (exp_profit - 1) / gamma
    hazrate_term = (f(alpha) ./ (1 - F(alpha) + 1e-8)) * (bidder_count(auc, config) - 1)

    ds_dc = hazrate_term * profit_term # Note: this should be a scalar

    return ds_dc
end

function ds_dc(
    auc::Auction,
    config::Config{R,A,Rt,H,E,S,Q,En},
    s,
    alpha
) where {R,A<:Mixed,Rt,H,E,S,Q,En}
    # Prerequisites
    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        auc.lowest_alpha,
        auc.alpha_bar
    )

    f(a) = pdf(alpha_dist, a)
    F(a) = cdf(alpha_dist, a)

    # Calculate optimal bid.
    opt = optimal_bid(auc, config, s, alpha)

    # Extract auction characteristics
    lambda = config.auctype.lambda
    qb = inflate(auc, config, opt)
    qe = auc.q_o
    sigma_sq = get_sigmasq(auc, config)
    gamma = get_gamma(auc, config, alpha)

    # Calculate db/ds
    dbds = db_ds(auc, config, s, alpha)

    lambda_b_min_c = (lambda * opt.bid) - (alpha * auc.c)
    mixed_q = (lambda * qb + (1 - lambda) * qe)

    profit_ce = opt.ce

    profit_term = exp(gamma * profit_ce) - 1

    numerator = (f(alpha) ./ (1 - F(alpha) - 1e-8)) * (bidder_count(auc, config) - 1) * profit_term
    denominator = dbds' * ((gamma .* mixed_q) - (gamma^2 .* sigma_sq .* lambda_b_min_c))

    deriv = numerator / denominator # Note: this should be a scalar
    return deriv
end

function density(auc::Auction, config::Config, a)
    # Assemble all the bits
    n_bidders = bidder_count(auc, config)

    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        auc.lowest_alpha,
        auc.alpha_bar,
    )

    F(a) = cdf(alpha_dist, a)
    f(a) = pdf(alpha_dist, a)

    return n_bidders * f(a) * ((1 - F(a))^(n_bidders - 1))
end

function equilibrium(
    auc::Auction,
    config::Config{R,A,Rt,H,E,S,Q,En};
    silent=false,
    verbose=true,
    plot_score=false,
    solution_folder=missing,
    return_sol_bid=false
) where {R,A,Rt,H,E,S,Q,En}
    if verbose
        println("Auction $(auc.contract_no), configuration $(String(config))")
    end

    # Determine whether to use AD
    use_ad = if config.solver isa ExactSolver
        true
    elseif config.auctype isa Lump
        true
    else
        false
    end

    # Actually do the stuff. try/catch as a lazyman.
    try
        # Calculate the boundary condition
        !silent && verbose && println("\t Calculating boundary condition")
        s_max = calc_boundary(auc, config)
        !silent && verbose && println("\t Boundary is $s_max")

        # Get alpha range
        a_max, a_min = alpha_span(auc, config)

        # Construct the ODE wrapper function
        f(s, p, alpha) = ds_dc(auc, config, s, alpha)

        # Check if there's only one entrant
        prob = if config.entry.n_entrants == 1
        

            # Monopolist s_max
            if config.monopoly isa FixedAmount
                function ce_fun(x)
                    ce = certainty_equivalent(auc, config, auc.alpha_bar, x)
                    return (ce - config.monopoly.amount)^2
                end

                res = Optim.optimize(ce_fun, 0, 100 * s_max, Optim.Brent())
                if Optim.converged(res)
                    s_max = res.minimizer
                end
            else
                s_max = monopoly_rent(auc, config)

            end

            prob = ODEProblem((args...) -> 0, s_max, (a_max, a_min))
            prob
        else
            # Construct ODE problem
            prob = ODEProblem(f, s_max, (a_max, a_min))
            prob
        end

        # Attempt to calculate a solution
        verbose && println("\t Solving ODE, autodiff=$(use_ad)")


        solvers = if use_ad
            if config.entry.n_entrants == 1
                [Rodas5(autodiff=true)]
            else
                [
                    # AutoTsit5(Rosenbrock23(autodiff=true)),
                    # TRBDF2(autodiff=true),
                    Rodas5(autodiff=true),
                    # KenCarp4(autodiff=true),
                    # Tsit5(thread=OrdinaryDiffEq.True()),
                    # Rosenbrock23(autodiff=true),
                    # Rodas4P(autodiff=true),
                    # Vern6(autodiff=true),
                ]
            end
        else
            [
                KenCarp47(autodiff=false),
                Rodas5(autodiff=false),
                # Rodas4(autodiff=false),
                # Tsit5(thread=OrdinaryDiffEq.False()),
                TRBDF2(autodiff=false),
                # Rosenbrock23(autodiff=false),
                # AutoTsit5(Rosenbrock23(autodiff=false)),
                # FBDF(autodiff=false),
                # QNDF(autodiff=false),
                # KenCarp4(autodiff=false),
            ]
            # [Rodas5(autodiff=false), ]
        end

        # solver = Rodas5()
        sol = missing

        # Check whether the solver is in the buffer folder
        solution_filename = "$(auc.contract_no)-$(config.tag).jld2"
        if false # !ismissing(solution_folder) && solution_filename in readdir(solution_folder)
            # Load the ODE if it exists
            verbose && @info "Loading existing ODE solution"
            JLD2.@load joinpath(solution_folder, solution_filename) sol
        else
            for tolerance in (1e-8, 1e-6, 1e-4)
                # for tolerance in (1e-12, 1e-10, 1e-8, 1e-6, 1e-4)
                for stopsize in (10, 100, 10_000, missing)
                    for solver in solvers
                        try
                            if verbose && (ismissing(sol) || sol.retcode != :Success)
                                println("""\t Trying ODE with tolerance $(tolerance), $(typeof(solver).name.name), and stepsize $stopsize.""")
                                if ismissing(sol)
                                    # println("\t No previous solution")
                                else
                                    println("\t Previous retcode was $(sol.retcode)")
                                end

                            end

                            if ismissing(sol) || sol.retcode != :Success
                                if ismissing(stopsize)
                                    sol = OrdinaryDiffEq.solve(
                                        prob,
                                        solver,
                                        # maxiters=1e7,
                                        verbose=false,
                                        reltol=tolerance,
                                        abstol=tolerance,
                                        dtmin=1e-20
                                    )
                                else
                                    stops = collect(range(a_max, a_min, length=stopsize))

                                    res = @timed sol = OrdinaryDiffEq.solve(
                                        prob,
                                        solver,
                                        # maxiters=1e7,
                                        verbose=false,
                                        reltol=tolerance,
                                        abstol=tolerance,
                                        tstops=stops,
                                        dtmin=1e-20
                                    )
                                end

                                if !ismissing(solution_folder) && !ismissing(sol) && sol.retcode == :Success
                                    if config.entry.n_entrants > 1
                                        JLD2.@save joinpath(solution_folder, solution_filename) sol
                                    end
                                    break
                                end
                            end
                        catch e
                            if e isa MathOptInterface.ResultIndexBoundsError ||
                               (e isa ErrorException &&
                                e.msg == "Expression contains an invalid NaN constant. This could be produced by `Inf - Inf`.") ||
                               (e isa ErrorException &&
                                e.msg == "Invalid coefficient NaN on variable b[1].")
                                println(e)
                                # Nothing
                            else
                                rethrow(e)
                            end
                        end
                    end
                end
            end
        end

        !silent && !verbose && config.entry.n_entrants > 1 && println(
                # "\t ",
                lpad(auc.contract_no, 10),
                lpad(config.tag, 55),
                lpad(sol.retcode, 20),
            )

        # Dispatch based on solution
        if sol.retcode == :Success
            # Plot the solution function
            if plot_score
                !silent && verbose && println("\t Writing plots")
                !isdir("plots") && mkpath("plots")
                plot(sol)
                savefig("plots/$(auc.contract_no)-$(config.tag).png")
            end

            # Construct various closures
            sol_bid(a) = optimal_bid(auc, config, sol(a), a)
            winning_cost_density(a) = density(auc, config, a) * sol_bid(a).cost
            winning_utility_density(a) = density(auc, config, a) * sol_bid(a).utility
            winning_ce_density(a) = density(auc, config, a) * sol_bid(a).ce

            # Get winner info
            winning_score = sol(auc.winning_alpha)
            winning_opt = sol_bid(auc.winning_alpha)

            winning_cost = winning_opt.cost
            winning_ce = winning_opt.ce
            winning_utility = winning_opt.utility

            # Get expectational parameters
            expected_ce = missing
            expected_cost = missing
            expected_utility = missing
            for max_eval in [EVALMAX]
                try
                    !silent && verbose && println("\t Calculating expected CE")
                    expected_ce = quadgk(
                        winning_ce_density,
                        a_min,
                        a_max;
                        maxevals=EVALMAX
                    )[1]
                    !silent && verbose && println("\t Calculating expected cost")
                    expected_cost = quadgk(
                        winning_cost_density,
                        a_min,
                        a_max;
                        maxevals=EVALMAX
                    )[1]
                    !silent && verbose && println("\t Calculating expected utility")
                    expected_utility = quadgk(
                        winning_utility_density,
                        a_min,
                        a_max;
                        maxevals=EVALMAX
                    )[1]
                    break
                catch eval_e
                    if eval_e isa InterruptException
                        rethrow(eval_e)
                    end
                    println("Trouble completing integration step with EVALMAX=$(max_eval), adjusting range")
                    println(eval_e)
                end
            end

            # Calculate the percentage of bids that aren't within 1e-4 of
            # the linear constraint
            linear_errors = []
            negative_bids = []
            if !(config.auctype isa Lump)
                for little_a in range(a_max, a_min, length=100)
                    little_opt = sol_bid(little_a)
                    push!(linear_errors, auc.q_o'little_opt.bid - sol(little_a))
                    push!(negative_bids, mean(little_opt.bid .< 0))
                end
            else
                push!(linear_errors, missing)
                push!(negative_bids, missing)
            end

            # Clear the memoization cache
            # empty!(memoize_cache(sol_bid))

            # Bundle up the results
            if !silent && verbose
                println("\t Solved:")
                println("\t\t expected_ce:      $expected_ce")
                println("\t\t expected_cost:    $expected_cost")
                println("\t\t expected_utility: $expected_utility")
                println("\t\t s_max:            $s_max")
                println("")
            end

            return (
                contract=auc.contract_no,
                winning_score=winning_score,
                winning_cost=winning_cost,
                winning_ce=winning_ce,
                winning_utility=winning_utility,
                expected_ce=expected_ce,
                expected_cost=expected_cost,
                expected_utility=expected_utility,
                s_max=s_max,
                linear_error=sum(linear_errors .^ 2),
                negative_bid=mean(negative_bids),
                tag=config.tag,
                retcode=sol.retcode,
            ), return_sol_bid ? sol_bid : sol
        else
            # Bundle up the results, with missing values.
            return (
                contract=auc.contract_no,
                winning_score=missing,
                winning_cost=missing,
                winning_ce=missing,
                winning_utility=missing,
                expected_ce=missing,
                expected_cost=missing,
                expected_utility=missing,
                s_max=s_max,
                linear_error=missing,
                negative_bid=missing,
                tag=config.tag,
                retcode=sol.retcode,
            ), (x -> missing)
        end
    catch e
        # rethrow(e)
        if e isa StackOverflowError
            open("stackoverflow-log.txt", "w") do io
                for mp in stacktrace()
                    println(io, mp)
                end
            end
            rethrow(e)
        end

        if e isa InterruptException || e isa MethodError ||
           e isa UndefVarError ||
           e isa StackOverflowError || e isa BoundsError
            rethrow(e)
        end

        println("bids.jl")
        println(e)

        retcode = if e isa DomainError
            :DomainError
        else
            Symbol(string(e))
        end

        println("\tReturning missing solution")

        return (
            contract=auc.contract_no,
            winning_score=missing,
            winning_cost=missing,
            winning_ce=missing,
            winning_utility=missing,
            expected_ce=missing,
            expected_cost=missing,
            expected_utility=missing,
            s_max=missing,
            linear_error=missing,
            negative_bid=missing,
            tag=config.tag,
            retcode=retcode,
        ), (x -> missing)
    end
end
