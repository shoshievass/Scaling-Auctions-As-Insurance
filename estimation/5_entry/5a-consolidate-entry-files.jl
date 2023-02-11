using DataFrames, CSV
using Statistics
using ProgressMeter

# Grab the project directory
project_dir = dirname(dirname(@__DIR__))
indir = joinpath(project_dir, "data", "estimation_step4_cf_inputs")
outdir = joinpath(project_dir, "data", "estimation_step5_outputs")
cf_name = "cfdata"
# potential_path = joinpath(indir, "potential_bidders_by_contract_v2.csv

function load_files()
    # List all the file
    path = get(
        ENV,
        "GROUP_SCRATCH",
        "../../data/"
    )
    path = joinpath(path, "estimation_step5_outputs", "raw")
    return filter(isfile, readdir(path, join=true))

    # folders = sort(filter(isdir, readdir(path, join=true)))

    # results = Dict{String,Vector{String}}()
    # for folder in folders
    #     results[folder] = 
    # end
    # return results
end

function parse_files()
    files = load_files()

    df = DataFrame()

    @showprogress for file in files
        df = vcat(df, DataFrame(CSV.File(file)))
    end

    # List all the file
    write_path = get(
        ENV,
        "GROUP_SCRATCH",
        "../../data/"
    )
    write_path = joinpath(write_path, "estimation_step5_outputs")
    !isdir(write_path) && mkpath(write_path)
    CSV.write(joinpath(write_path, "step5_entry_calibration_output.csv"), df)

    return df
end

parse_files()
