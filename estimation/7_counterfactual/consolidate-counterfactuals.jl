using DataFrames, CSV

function list_files()
    # Get the location of the files to load
    project_dir = dirname(dirname(@__DIR__))
    path = get(ENV, "GROUP_SCRATCH", joinpath(project_dir, "data", "estimation_step7_counterfactuals"))

    for dir in filter(isdir, readdir(path, join=true))
        println(dir)
        monopath = joinpath(dir, "monopoly-info")
        resultpath = joinpath(dir, "raw")

        mono = DataFrame()
        for path in readdir(monopath, join=true)
            println("\t $path")
            ndf = DataFrame(CSV.File(path))
            try
                mono = vcat(mono, ndf)
            catch e
                println(e)
            end
        end

        results = DataFrame()
        for path in readdir(resultpath, join=true)
            println("\t $path")
            ndf = DataFrame(CSV.File(path))
            try
                results = vcat(results, ndf)
            catch e
                println(e)
            end
        end

        CSV.write(joinpath(dir, "aggregate_results.csv"), mono)
        CSV.write(joinpath(dir, "detailed_results.csv"), results)
    end
end

list_files()
