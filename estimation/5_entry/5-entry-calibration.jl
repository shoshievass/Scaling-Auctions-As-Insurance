using Pkg;
Pkg.activate(dirname(@__DIR__));
Pkg.instantiate();
using Revise
using Counterfactual
using Distributed
using ThreadsX
using Random
using Dates

# Load preamble and auction cofngurations 
println("Loading preamble...")
include("../auxiliary/preamble-entry.jl")
println("Preamble loaded...")
include("../auxiliary/auction-configs.jl")

# Grab the project directory
project_dir = dirname(dirname(@__DIR__))
indir = joinpath(project_dir, "data", "estimation_step4_cf_inputs")
outdir = joinpath(project_dir, "data", "estimation_step5_outputs")
cf_name = "cfdata"
# potential_path = joinpath(indir, "potential_bidders_by_contract_v2.csv")

@info "Path information" project_dir indir outdir cf_name

# Load the auction data
function go(job_id, total_jobs)
    # Read the auction data in from disk
    println("Loading auction data")
    auctions, instrument_info, Z = load_auctions(indir, outdir, cf_name)

    println("Creating output folder")
    master_id = get(ENV, "SLURM_ARRAY_JOB_ID", "")
    path = get(ENV, "GROUP_SCRATCH", "../../data/estimation_step5_outputs/raw/") # Use local path if no group scratch
    path = joinpath(path, master_id) # Uses slurm job id if available
    !isdir(path) && mkpath(path)

    # Determine whether to use actual quantities
    use_actual = parse(Bool, get(ENV, "USE_ACTUAL", "false"))

    # Define lambda and K grids
    lambda_grid = vcat(
        collect(range(0, 1.0, step=0.05)), # Real
        [1.25, 1.5, 1.75, 2.0],
    )

    k_grid = vcat(
        map(exp, range(log(0.0001), 0, length=50)), # Real
        [1.25, 1.5, 1.75, 2.0, 3.0, 4.0, 5.0, 10.0],
    )

    # k_grid = map(exp, range(log(0.00001), log(0.00125146), length=30))

    @info "Grid information" length(k_grid) length(lambda_grid)

    # Get all bin ids
    bins = unique(map(x -> x.bin_id, auctions))

    # Determine which auctions this proc will be doing
    auc_ids = partition_jobs(auctions, total_jobs)[job_id]

    # Set up a vector to store results
    results = []
    results_lock = Threads.SpinLock()

    # Go through each auction
    for auc in auc_ids
        # Iterate through lambdas (global)
        for lambda in lambda_grid
            # config = reparameterize(baseline(auc), 0.05, lambda, ScaledMonopoly(1.25))
            # (a_opt, k_opt), _, _ = solve_alpha_bar_and_k(auc, config)

            #Iterate through K (bin)
            Threads.@threads for k in k_grid
                # for k in [k_opt]
                # Make a new auction
                newauc = deepcopy(auc)

                # Configure the file
                start_time = now()
                config = use_actual ?
                         reparameterize(baseline_actual(newauc), k, lambda, ScaledMonopoly(1.25)) :
                         reparameterize(baseline(newauc), k, lambda, ScaledMonopoly(1.25))

                # Solve for alpha bar
                alpha_bar, alpha_bar_norm, exante_ce = solve_alpha_bar(newauc, config)
                newauc.alpha_bar = alpha_bar

                # Solve for entry by non-monopolist
                auc_N = newauc.max_num_bidders
                entry_prob = newauc.num_bidders / auc_N

                eqs = []
                solutions = []
                _inner_lock = Threads.ReentrantLock()

                Threads.@threads for n in 1:auc_N
                    # Generate configuration
                    config = configurate(newauc, lambda, n; entry_cost=k)

                    # Calculate equilibrium
                    eq, sol = equilibrium(newauc, config; silent=false, verbose=false)

                    if eq.retcode == :Success
                        lock(_inner_lock) do
                            push!(solutions, sol)
                            push!(eqs, eq)
                        end
                    else
                        lock(_inner_lock) do
                            push!(solutions, x -> missing)
                            push!(eqs, missing)
                        end
                    end
                end

                ################
                # Uniform norm #
                ################
                uniform_norm = (mean ∘ skipmissing ∘ map)(
                    a -> exante_alpha(newauc, lambda, a, solutions, auc_N, entry_prob, k)[1] .>= 0,
                    get_alphas(newauc, config, UniformAlpha(1000))
                )

                ##############
                # Entry norm #
                ##############
                alpha_dist = truncated(
                    LogNormal(newauc.alpha_lgmean, newauc.alpha_lgsigma),
                    # 0.5,
                    auc.lowest_alpha,
                    newauc.alpha_bar_bar,
                )

                arange = get_alphas(newauc, config, RandomAlpha(1000))

                exante = (map)(
                    a -> exante_alpha(newauc, lambda, a, solutions, auc_N, entry_prob, k),
                    arange
                )

                interpolations = (mean ∘ skipmissing ∘ map)(x -> x[2], exante)
                entry_norm = (mean ∘ skipmissing ∘ map)(x -> x[1] >= 0, exante)

                ############
                # CDF norm #
                ############
                cdf_norm = cdf(alpha_dist, alpha_bar)

                ####################
                # Save the results #
                ####################
                stop_time = now()
                duration = round(stop_time - start_time, Second)
                nt = (
                    contract_no=newauc.contract_no,
                    lambda=lambda,
                    k=k,
                    alpha_bar=alpha_bar,
                    alpha_bar_bayes=auc.alpha_bar,
                    alpha_bar_bar=auc.alpha_bar_bar,
                    alpha_bar_cdf=cdf_norm,
                    alpha_bar_exante=exante_ce,
                    eps=exante_ce < 0 ? -exante_ce : 0,
                    net_entry=exante_ce < 0 ? k - exante_ce : k,
                    q=entry_prob,
                    entry_pct=entry_norm,
                    uniform_pct=uniform_norm,
                    alpha_bar_norm=alpha_bar_norm,
                    entry_norm=(entry_norm - entry_prob)^2,
                    cdf_norm=(cdf_norm - entry_prob)^2,
                    uniform_norm=(cdf_norm - uniform_norm)^2,
                    ewo=auc.extra_work_payment,
                    entry_interpolations=interpolations,
                    num_bidders=newauc.num_bidders,
                    potential_bidders=auc_N,
                    highest_alpha=newauc.highest_alpha,
                    lowest_alpha=newauc.lowest_alpha,
                    min_alpha=newauc.alpha_min_bin,
                    duration=duration,
                    lower_than_lowest=alpha_bar < newauc.lowest_alpha,
                    lower_than_highest=alpha_bar < newauc.highest_alpha,
                    actual=use_actual,
                )

                lock(results_lock) do
                    push!(results, nt)

                    df = sort(DataFrame(results), [:contract_no, :lambda, :k, :actual])
                    fn = use_actual ?
                         joinpath(path, "$(auc.contract_no)-actual.csv") :
                         joinpath(path, "$(auc.contract_no)-estimated.csv")

                    CSV.write(fn, df)
                end
            end
        end
    end

    return "all good"
end

# You can estimate a subset of auctions by adjusting total job count and 
# The current job id. Each job is assigned approximated 430 / total_jobs
# auctions, so setting the default slrurm array task count to 
# 215 will assign two auctions per job.
total_jobs = parse(Int, get(ENV, "SLURM_ARRAY_TASK_COUNT", "430"))
current_job = parse(Int, get(ENV, "SLURM_ARRAY_TASK_ID", "1"))

# Estimate entry results.
res = go(current_job, total_jobs)
