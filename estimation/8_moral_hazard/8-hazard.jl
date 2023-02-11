
using Pkg;
Pkg.activate(dirname(@__DIR__))
using Revise
using Counterfactual
# using Distributed


using Dates
using DataFrames, CSV
using Statistics

# Notify the user about thread count
println("Using $(Threads.nthreads()) threads")

include("../auxiliary/preamble-entry.jl")
include("mh-auction-configs.jl")

# Grab the project directory
project_dir = dirname(dirname(@__DIR__))
indir = joinpath(project_dir, "data", "estimation_step4_cf_inputs")
outdir = joinpath(project_dir, "data", "estimation_step5_outputs")
cf_name = "cfdata"
@info "directories" project_dir indir outdir

function hazard_report(auc, config, sol)
    # Calculate the solution for the winner
    alpha = auc.winning_alpha
    opt = sol(alpha)
    ids = auc.item_ids
    bid = !(config.auctype isa Lump) ? opt.bid : repeat([missing], length(ids))
    q_actual = auc.q_a
    q_estimated = auc.q_a_model
    q_engineer = auc.q_o
    cost = auc.c
    shade = !(config.auctype isa Lump) ? opt.shade : repeat([missing], length(ids))
    shade_amt = Counterfactual.get_ybar(auc, config)

    cap = config.hazard isa NoHazard ? missing : config.hazard.cap
    shade_cap = config.hazard isa NoHazard ? missing : config.hazard.n_items

    # Generate a bunch of tuples
    tups = []
    for i in eachindex(ids)
        tup = (
            contract_no = auc.contract_no,
            tag = config.tag,
            item_id = ids[i],
            q_actual = q_actual[i],
            q_estimated = q_estimated[i],
            q_engineer = q_engineer[i],
            bid = bid[i],
            cost = cost[i],
            shade = shade[i],
            y_bar = shade_amt[i],
            shade_cap = cap,
            shade_item_cap = shade_cap,
        )

        push!(tups, tup)
    end

    return DataFrame(tups)
end

function run_auc(auc, config_fns, root, config_df)
    results = []
    outpath = joinpath(root, "$(auc.contract_no).csv")
    hazardpath = joinpath(root, "hazard", "$(auc.contract_no).csv")
    !ispath(dirname(hazardpath)) && mkdir(dirname(hazardpath))

    if isfile(outpath)
        # return results
    end

    hazards = DataFrame()

    # Thread locks
    hazardslock = Threads.SpinLock()
    resultslock = Threads.SpinLock()

    for config_func in config_fns
        config = config_func(auc)
        isactual = config.quantity isa Counterfactual.ActualPrediction
        row = filter(
            x -> x.contract_no == auc.contract_no && x.actual == isactual,
            config_df
        )

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
        println(config.tag)

        stats = @timed res, sol = equilibrium(
            auc,
            config,
            plot_score=false,
            verbose=true,
            return_sol_bid=true
        )

        if res.retcode == :Success
            hazard = hazard_report(auc, config, sol)
            # display(hazard)
            lock(hazardslock) do 
                hazards = vcat(hazards, hazard)
                CSV.write(hazardpath, hazards)
            end
        end

        lock(resultslock) do
            push!(results, res)
            CSV.write(outpath, DataFrame(results))
        end
    end

    df = DataFrame(results)

    @info "" outpath hazardpath
    CSV.write(outpath, df)
    CSV.write(hazardpath, hazards)

    return results
end

function go(job_id, total_jobs)
    # Read the auction data in from disk
    auctions, _... = load_auctions(indir, outdir, cf_name)

    # Get which norm file to use
    norm_file = get(ENV, "NORM_FILE", "calibrated_entry_params_binlevel")

    # Load the lambdas and ks
    config_file = joinpath(project_dir, "data", "estimation_step6_precf", "$(norm_file).csv")
    config_df = DataFrame(CSV.File(config_file))

    # Set up result arrays
    println("Creating output folder")
    path = get(ENV, "GROUP_SCRATCH", joinpath(project_dir, "data", "estimation_step8_moralhazard"))
    path = joinpath(path, norm_file)
    !isdir(path) && mkpath(path)

    # Get all bin ids
    bins = unique(map(x -> x.bin_id, auctions))

    for auc in auctions
        auc.alpha_bar = min(auc.highest_alpha * 1.2, 3.0)
    end

    # Determine which auctions this proc will be doing
    auc_ids = partition_jobs(auctions, total_jobs)[job_id]

    results_vec = map(
        a -> run_auc(a, config_fns, path, config_df),
        auc_ids
    )
    #results_df = DataFrame(reduce(vcat, results_vec))

    #CSV.write("mh-results.csv", results_df)

    #return results_df
end

total_jobs = parse(Int, get(ENV, "SLURM_ARRAY_TASK_COUNT", "430"))
current_job = parse(Int, get(ENV, "SLURM_ARRAY_TASK_ID", "1"))

results = go(current_job, total_jobs)
