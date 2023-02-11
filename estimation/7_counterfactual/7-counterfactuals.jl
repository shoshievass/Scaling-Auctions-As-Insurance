using Pkg;
Pkg.activate(dirname(@__DIR__));
Pkg.instantiate();
using Revise
using Counterfactual
using Distributed
using ThreadsX
using Random

# Load preamble and auction configurations 
println("Loading preamble...")
include("../auxiliary/preamble-entry.jl")
println("Preamble loaded...")
include("../auxiliary/auction-configs.jl")

# Grab the project directory
project_dir = dirname(dirname(@__DIR__))
indir = joinpath(project_dir, "data", "estimation_step4_cf_inputs")
outdir = joinpath(project_dir, "data", "estimation_step5_outputs")
cf_name = "cfdata"
@info "directories" project_dir indir outdir

function expected_cost(auc_original, config, alpha_bar, k, l, g_thing, is_prime)
    auc = deepcopy(auc_original)
    auc.alpha_bar = alpha_bar

    # Solve for entry by non-monopolist
    auc_N = auc.max_num_bidders
    entry_prob = auc.num_bidders / auc_N

    # Calculate q
    alpha_dist = truncated(
        LogNormal(auc.alpha_lgmean, auc.alpha_lgsigma),
        # 0.5,
        auc.lowest_alpha,
        auc.alpha_bar_bar,
    )
    q_prime = cdf(alpha_dist, alpha_bar)

    # Calculate outcome weights
    θs = [binomial(auc_N - 1, m - 1) * q_prime^(m - 1) * (1 - q_prime)^(auc_N - m) for m in 1:(auc_N)]
    costs = zeros(Union{Float64,Missing}, auc_N)
    ces = zeros(Union{Float64,Missing}, auc_N)
    utils = zeros(Union{Float64,Missing}, auc_N)

    # Integrate across auctions
    _inner_lock = Threads.ReentrantLock()
    Threads.@threads for n in 1:auc_N
        # Generate configuration
        config = reparameterize(config, k, l, g_thing; n=n)

        # Calculate equilibrium
        println("    Trying to solve equilibrium $n of $(auc_N)")
        eq, sol = equilibrium(auc, config; silent=true, verbose=true)

        if eq.retcode == :Success
            lock(_inner_lock) do
                costs[n] = eq.expected_cost
                ces[n] = eq.expected_ce
                utils[n] = eq.expected_utility
            end
        else
            lock(_inner_lock) do
                costs[n] = missing
                ces[n] = missing
                utils[n] = missing
            end
        end
    end

    # Interpolate the results
    println("    Interpolating results")
    interpolated_costs, num_interpolated = polyinterp(1:auc_N, costs)
    interpolated_ces, num_interpolated_ce = polyinterp(1:auc_N, ces)
    isactual = config.quantity isa Counterfactual.ActualPrediction

    # Make a buncha tuples
    println("    Compiling results in tuples")
    tups = map(1:auc_N) do i
        (
            contract_no=auc.contract_no,
            tag=config.tag,
            lambda=l,
            k=k,
            n=i,
            N=auc_N,
            observed_bidders=auc.num_bidders,
            expected_cost=interpolated_costs[i],
            expected_ce=interpolated_ces[i],
            raw_expected_cost=costs[i],
            raw_expected_ce=ces[i],
            weighting=θs[i],
            actual=isactual,
            is_prime=is_prime,
        )
    end

    # @info "" q_prime alpha_bar
    # display([interpolated_costs θs])
    # display(tups)

    return tups
end

function go(job_id, total_jobs)
    # Set the seed
    Random.seed!(15)

    # Get which norm file to use
    norm_file = get(ENV, "NORM_FILE", "calibrated_entry_params_binlevel")

    # Read the auction data in from disk
    println("Loading auction data")
    auctions, _, _ = load_auctions(indir, outdir, cf_name)

    # Find the project directory
    println("Finding project directory")
    # project_dir = dirname(@__DIR__)

    println("Creating output folder")
    master_id = get(ENV, "SLURM_ARRAY_JOB_ID", "")
    master_id = length(master_id) > 0 ? string(master_id, "-", norm_file) : norm_file
    path = get(ENV, "GROUP_SCRATCH", joinpath(project_dir, "data", "estimation_step7_counterfactuals"))
    path = joinpath(path, norm_file)
    !isdir(path) && mkpath(path)

    monopath = joinpath(path, "monopoly-info")
    resultpath = joinpath(path, "results")
    rawpath = joinpath(path, "raw")
    !isdir(monopath) && mkpath(monopath)
    !isdir(resultpath) && mkpath(resultpath)
    !isdir(rawpath) && mkpath(rawpath)

    # Load the lambdas and ks
    config_file = joinpath(project_dir, "data", "estimation_step6_precf", "$(norm_file).csv")
    config_df = DataFrame(CSV.File(config_file))

    # Construct a bunch of configurations
    tups = []
    orig_eq_results = []
    eq_results = []

    # Determine which auctions this proc will be doing
    auc_ids = partition_jobs(1:length(auctions), total_jobs)[job_id]

    for auc_id in auc_ids
        # Grap the auction
        auc = auctions[auc_id]
        alpha_bar = auc.alpha_bar

        baseline_actual = missing
        baseline_actual_norisk = missing
        baseline_estimated = missing
        baseline_estimated_norisk = missing

        actual_cost = missing
        estimated_cost = missing

        # Set the entry cost and renegotiation proportion for each configuration
        for config_fn in config_fns
            # Configurate!
            config = config_fn(auc)
            println(config.tag)
            isactual = config.quantity isa Counterfactual.ActualPrediction

            row = filter(x -> x.contract_no == auc.contract_no && x.actual == isactual, config_df)

            if size(row, 1) == 0
                continue
            end

            l = row[1, :lambda]
            k = row[1, :k]
            g = 1.25

            # Check if it's a lump sum.
            g_thing = ScaledMonopoly(g)

            # RECONFIGURATE
            config = reparameterize(config, k, l, g_thing)

            # Estimate the equilibrium, no change in alpha_bar
            # orig_eq, _ = equilibrium(
            #     auc,
            #     config,
            #     plot_score=false,
            #     verbose=false,
            #     silent=true
            # )

            # Calculate the new alpha_bar
            println("    Calculating alpha bar prime")
            alpha_bar_prime, _, monopolist_ce, opt = solve_alpha_bar(auc, config; use_cdf=false)

            println("    Calculating expected costs under original alpha bar")
            alpha_bar_tups = expected_cost(auc, config, auc.alpha_bar, k, l, g_thing, false)
            println("    Calculating expected costs under alpha bar prime")
            alpha_bar_prime_tups = expected_cost(auc, config, alpha_bar_prime, k, l, g_thing, true)

            cost = mapreduce(x -> x.weighting * x.expected_cost, +, alpha_bar_tups)
            cost_prime = mapreduce(x -> x.weighting * x.expected_cost, +, alpha_bar_prime_tups)

            ts = DataFrame(vcat(alpha_bar_tups, alpha_bar_prime_tups))
            CSV.write(joinpath(rawpath, "$(config.tag)-$job_id.csv"), ts)
            # display(ts)

            # Calculate office score
            os = office_score(auc)

            # Check to save the baseline/baseline_norisk cases
            if config.tag == "baseline"
                baseline_estimated = monopolist_ce
                estimated_cost = cost
            elseif config.tag == "baseline-actual"
                baseline_actual = monopolist_ce
                actual_cost = cost
            elseif config.tag == "norisk-actual"
                baseline_actual_norisk = monopolist_ce
            elseif config.tag == "norisk-estimated"
                baseline_estimated_norisk = monopolist_ce
            end

            # Get relevant cost
            basecost = if isactual
                actual_cost
            else
                estimated_cost
            end

            # Save named tuple with solution info
            nt = (
                contract_no=auc.contract_no,
                tag=config.tag,
                prime_cost=cost_prime,
                reg_cost=cost,
                pct_diff=!ismissing(cost_prime) && cost_prime > 0 ? basecost / cost_prime - 1 : missing,
                dollar_diff=basecost - cost_prime,
                k=k,
                lambda=l,
                alpha_bar=alpha_bar,
                alpha_bar_prime=alpha_bar_prime,
                lowest_alpha=auc.lowest_alpha,
                highest_alpha=auc.highest_alpha,
                alpha_bar_bar=auc.alpha_bar_bar,
                monopoly_scalar=g,
                monopoly_rent=monopoly_rent(auc, config),
                monopolist_ce=monopolist_ce,
                office_score=os,
                prime_savings=cost_prime - cost,
                prime_savings_pct=!ismissing(cost) && cost > 0 ? (cost_prime / cost) - 1 : missing,
                lower_than_lowest=alpha_bar_prime < auc.lowest_alpha,
                lower_than_highest=alpha_bar_prime < auc.highest_alpha,
                is_actual=isactual,
            )
            push!(tups, nt)
        end
    end

    monopoly_info = DataFrame(tups)
    # results = DataFrame(vcat(eq_results, orig_eq_results))

    # Write to disk
    size(monopoly_info, 2) > 0 && CSV.write(joinpath(monopath, "$job_id.csv"), monopoly_info)
    # size(results, 2) > 0 && CSV.write(joinpath(resultpath, "$job_id.csv"), rename(results, :contract => :contract_no))

    return monopoly_info
end

# Run it, homie
total_jobs = parse(Int, get(ENV, "SLURM_ARRAY_TASK_COUNT", "1"))
current_job = parse(Int, get(ENV, "SLURM_ARRAY_TASK_ID", "1"))
monopoly_info = go(current_job, total_jobs)

# display(sort(monopoly_info, [:is_actual, :prime_cost]))
println("GO TEAM")
