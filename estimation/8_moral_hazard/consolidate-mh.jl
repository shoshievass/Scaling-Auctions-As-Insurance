using Pkg;
Pkg.activate(dirname(@__DIR__))

using DataFrames, CSV

# Grab the project directory
project_dir = dirname(dirname(@__DIR__))
indir = joinpath(project_dir, "data", "estimation_step4_cf_inputs")
outdir = joinpath(project_dir, "data", "estimation_step8_moralhazard")
cf_name = "cfdata"
@info "directories" project_dir indir outdir

function list_files()
    local results
    for dir in filter(isdir, sort(readdir(outdir, join=true), rev=false))
        println(dir)


        results = DataFrame()
        for path in readdir(dir, join=true)
            if endswith(path, "hazard")
                @info "Found hazard path" path
                hazard_files = filter(
                    x -> endswith(x, ".csv") && x != "hazard.csv",
                    readdir(path, join=true)
                )

                hazard_df = DataFrame()
                for hazard_file in hazard_files
                    ndf = DataFrame(CSV.File(hazard_file, silencewarnings=true))
                    try
                        if length(names(hazard_df)) == 0
                            hazard_df = ndf
                        elseif length(names(ndf)) == 0
                            # 
                        else
                            cols = intersect(names(hazard_df), names(ndf))
                            hazard_df = vcat(hazard_df[:,cols], ndf[:,cols])
                        end
                    catch e
                        rethrow(e)
                        println(e)
                    end 
                end

                # Write the hazard report to disk
                CSV.write(joinpath(path, "hazard.csv"), hazard_df)
            end
            
            println("\t $path")
            if endswith(path, ".csv")
                ndf = DataFrame(CSV.File(path, silencewarnings=true))
                try
                    if length(names(results)) == 0
                        results = ndf
                    elseif length(names(ndf)) == 0
                        # 
                    else
                        cols = intersect(names(results), names(ndf))
                        results = vcat(results[:,cols], ndf[:,cols])
                    end
                catch e
                    rethrow(e)
                    println(e)
                end
            end
        end
        # return results

        results[!, "directory"] .= basename(dir)

        CSV.write(joinpath(outdir, "mh_results_nocap.csv"), results)
    end

    return results
end

results = list_files()
