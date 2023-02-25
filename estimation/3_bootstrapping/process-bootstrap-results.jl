# Imports
import Pkg;
Pkg.activate(@__DIR__);
using DataFrames, CSV
using Dates
using StatsBase
using Random
using Statistics

# Set up paths
project_dir = @__DIR__
estimation_results_dir = joinpath(project_dir, "..", "..", "data", "estimation_step3")

# Include helper scripts
include(joinpath(project_dir, "CARA_Estimation_Structs.jl"))


stage1_id = 1
stage2_id = 1
bootstrap_out_dir = joinpath(estimation_results_dir, "second_stage_estimates_bootstrap_raw_results")


function enter_sumstat_into_dict!(dict_input, statname, stat)
    try
        push!(dict_input[statname], stat)
    catch error
        if isa(error, KeyError)
            dict_input[statname] = [stat]
        end
    end
end

function enter_bootstrap_stats_into_sumdict!(incoming_stat_dict,
    incoming_stat_name,
    bs_mean_dict_input,
    bs_median_dict_input,
    bs_sd_dict_input,
    bs_ptiles_dict_input,
    bs_trunc_sd_dict_input,
    bs_lower_95ci_dict_input,
    bs_upper_95ci_dict_input,
    trunc95_numdraws
)

    for a in keys(incoming_stat_dict)
        a_var = Statistics.var(incoming_stat_dict[a])
        a_mean = Statistics.mean(incoming_stat_dict[a])
        a_median = Statistics.median(incoming_stat_dict[a])
        a_ptiles = [Statistics.quantile(incoming_stat_dict[a], p) for p in [0:0.01:1]]
        a_lower_95ci = Statistics.quantile(incoming_stat_dict[a], 0.025)
        a_upper_95ci = Statistics.quantile(incoming_stat_dict[a], 0.975)

        stat_draws = incoming_stat_dict[a]
        sort!(stat_draws)
        trunc_stat_draws = stat_draws[trunc95_numdraws:(length(stat_draws)-trunc95_numdraws)]
        a_trunc_var = Statistics.var(trunc_stat_draws)

        bs_mean_dict_input[incoming_stat_name*"_$a"] = a_mean
        bs_median_dict_input[incoming_stat_name*"_$a"] = a_median
        bs_sd_dict_input[incoming_stat_name*"_$a"] = sqrt(a_var)
        bs_ptiles_dict_input[incoming_stat_name*"_$a"] = a_ptiles
        bs_trunc_sd_dict_input[incoming_stat_name*"_$a"] = sqrt(a_trunc_var)
        bs_lower_95ci_dict_input[incoming_stat_name*"_$a"] = a_lower_95ci
        bs_upper_95ci_dict_input[incoming_stat_name*"_$a"] = a_upper_95ci
    end
end

function enter_param_bootstrap_stats_into_sumdict!(incoming_stat_dict,
    incoming_stat_name,
    break_bidder_contract_up,
    bs_mean_dict_input,
    bs_median_dict_input,
    bs_sd_dict_input,
    bs_ptiles_dict_input,
    bs_trunc_sd_dict_input,
    bs_lower_95ci_dict_input,
    bs_upper_95ci_dict_input,
    trunc95_numdraws
)

    for a in keys(incoming_stat_dict)
        a_var = Statistics.var(incoming_stat_dict[a])
        a_mean = Statistics.mean(incoming_stat_dict[a])
        a_median = Statistics.median(incoming_stat_dict[a])
        a_ptiles = [Statistics.quantile(incoming_stat_dict[a], p) for p in [0:0.01:1]]
        a_lower_95ci = Statistics.quantile(incoming_stat_dict[a], 0.025)
        a_upper_95ci = Statistics.quantile(incoming_stat_dict[a], 0.975)

        stat_draws = incoming_stat_dict[a]
        sort!(stat_draws)
        trunc_stat_draws = stat_draws[trunc95_numdraws:(length(stat_draws)-trunc95_numdraws)]
        a_trunc_var = Statistics.var(trunc_stat_draws)

        if break_bidder_contract_up
            bidder_id = a[2]
            contract_no = a[1]
            stat_name = "$contract_no" * "_$bidder_id"
        else
            stat_name = "$a"
        end

        bs_mean_dict_input[incoming_stat_name*"_$stat_name"] = a_mean
        bs_median_dict_input[incoming_stat_name*"_$stat_name"] = a_median
        bs_sd_dict_input[incoming_stat_name*"_$stat_name"] = sqrt(a_var)
        bs_ptiles_dict_input[incoming_stat_name*"_$stat_name"] = a_ptiles
        bs_trunc_sd_dict_input[incoming_stat_name*"_$stat_name"] = sqrt(a_trunc_var)
        bs_lower_95ci_dict_input[incoming_stat_name*"_$stat_name"] = a_lower_95ci
        bs_upper_95ci_dict_input[incoming_stat_name*"_$stat_name"] = a_upper_95ci
    end

end

function go()
    bs_alpha_i_dict = Dict{Any,Any}()
    bs_beta_alpha_dict = Dict{Any,Any}()
    bs_beta_gamma_dict = Dict{Any,Any}()
    bs_gamma_intercept_dict = Dict{Any,Any}()

    bs_gamma_dict = Dict{Any,Any}()
    bs_alpha_dict = Dict{Any,Any}()
    bs_mkup_dict = Dict{Any,Any}()

    bs_sumstat_gamma_dict = Dict{Any,Any}()
    bs_sumstat_alpha_dict = Dict{Any,Any}()
    bs_sumstat_mkup_dict = Dict{Any,Any}()

    for stage1_id in 1:100
        out_dir = joinpath(bootstrap_out_dir, "bs_" * "$stage1_id")

        for stage2_id in 1:100
            println("Stage 1: $stage1_id, Stage 2: $stage2_id")
            writepath = joinpath(out_dir, "$stage2_id" * "_mkp_df.csv")
            mkup_df = DataFrame(CSV.File(writepath))

            ## Final Parameters (Medians)
            gamma_ests = mkup_df[!, :gamma]
            alpha_ests = mkup_df[!, :alpha]
            mkp_ests = mkup_df[!, :mkp]

            gamma_ests = sort(gamma_ests)
            alpha_ests = sort(alpha_ests)
            mkp_ests = sort(mkp_ests)

            trunc99_numdraws = Int(round(length(mkup_df[!, :gamma]) / 100))
            trunc_gammas = gamma_ests[trunc99_numdraws:(length(gamma_ests)-trunc99_numdraws)]
            trunc_alphas = alpha_ests[trunc99_numdraws:(length(alpha_ests)-trunc99_numdraws)]
            trunc_mkps = mkp_ests[trunc99_numdraws:(length(mkp_ests)-trunc99_numdraws)]

            mean_gamma = mean(mkup_df[!, :gamma])
            mean_alpha = mean(mkup_df[!, :alpha])
            mean_mkp = mean(mkup_df[!, :mkp])

            med_gamma = median(mkup_df[!, :gamma])
            med_alpha = median(mkup_df[!, :alpha])
            med_mkp = median(mkup_df[!, :mkp])

            p25_gamma = quantile(mkup_df[!, :gamma], 0.25)
            p25_alpha = quantile(mkup_df[!, :alpha], 0.25)
            p25_mkp = quantile(mkup_df[!, :mkp], 0.25)

            p75_gamma = quantile(mkup_df[!, :gamma], 0.75)
            p75_alpha = quantile(mkup_df[!, :alpha], 0.75)
            p75_mkp = quantile(mkup_df[!, :mkp], 0.75)

            trunc_mean_gamma = mean(trunc_gammas)
            trunc_mean_alpha = mean(trunc_alphas)
            trunc_mean_mkp = mean(trunc_mkps)

            trunc_med_gamma = median(trunc_gammas)
            trunc_med_alpha = median(trunc_alphas)
            trunc_med_mkp = median(trunc_mkps)

            trunc_p25_gamma = quantile(trunc_gammas, 0.25)
            trunc_p25_alpha = quantile(trunc_alphas, 0.25)
            trunc_p25_mkp = quantile(trunc_mkps, 0.25)

            trunc_p75_gamma = quantile(trunc_gammas, 0.75)
            trunc_p75_alpha = quantile(trunc_alphas, 0.75)
            trunc_p75_mkp = quantile(trunc_mkps, 0.75)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "mean", mean_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "mean", mean_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "mean", mean_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "median", med_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "median", med_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "median", med_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "p25", p25_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "p25", p25_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "p25", p25_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "p75", p75_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "p75", p75_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "p75", p75_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "trunc_mean", trunc_mean_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "trunc_mean", trunc_mean_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "trunc_mean", trunc_mean_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "trunc_median", trunc_med_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "trunc_median", trunc_med_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "trunc_median", trunc_med_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "trunc_p25", trunc_p25_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "trunc_p25", trunc_p25_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "trunc_p25", trunc_p25_mkp)

            enter_sumstat_into_dict!(bs_sumstat_gamma_dict, "trunc_p75", trunc_p75_gamma)
            enter_sumstat_into_dict!(bs_sumstat_alpha_dict, "trunc_p75", trunc_p75_alpha)
            enter_sumstat_into_dict!(bs_sumstat_mkup_dict, "trunc_p75", trunc_p75_mkp)

        end
    end

    bs_mean_dict = Dict{Any,Any}()
    bs_median_dict = Dict{Any,Any}()
    bs_sd_dict = Dict{Any,Any}()
    bs_ptiles_dict = Dict{Any,Any}()
    bs_trunc_sd_dict = Dict{Any,Any}()
    bs_lower_95ci_dict = Dict{Any,Any}()
    bs_upper_95ci_dict = Dict{Any,Any}()

    trunc95_numdraws = Int(round((100 * 100) / 40))



    enter_bootstrap_stats_into_sumdict!(bs_sumstat_gamma_dict,
        "gamma",
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    enter_bootstrap_stats_into_sumdict!(bs_sumstat_alpha_dict,
        "alpha",
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    enter_bootstrap_stats_into_sumdict!(bs_sumstat_mkup_dict,
        "mkp",
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    sort!(collect(bs_lower_95ci_dict))
    sort!(collect(bs_median_dict))

    ## get original parameter estimates ##
    estimate_id = joinpath(bootstrap_out_dir, "bs_0", "0")

    param_sum_dict = Dict{Any,Any}()

    mkup_df = DataFrame(CSV.File(estimate_id * "_mkp_df.csv"));

    ## Estimated Params 

    gamma_ests = mkup_df[!, :gamma]
    alpha_ests = mkup_df[!, :alpha]
    mkp_ests = mkup_df[!, :mkp]

    gamma_ests = sort(gamma_ests)
    alpha_ests = sort(alpha_ests)
    mkp_ests = sort(mkp_ests)

    trunc99_numdraws = Int(round(length(mkup_df[!, :gamma]) / 100))
    trunc_gammas = gamma_ests[trunc99_numdraws:(length(gamma_ests)-trunc99_numdraws)]
    trunc_alphas = alpha_ests[trunc99_numdraws:(length(alpha_ests)-trunc99_numdraws)]
    trunc_mkps = mkp_ests[trunc99_numdraws:(length(mkp_ests)-trunc99_numdraws)]

    param_sum_dict["gamma_mean"] = mean(mkup_df[!, :gamma])
    param_sum_dict["alpha_mean"] = mean(mkup_df[!, :alpha])
    param_sum_dict["mkp_mean"] = mean(mkup_df[!, :mkp])

    param_sum_dict["gamma_median"] = median(mkup_df[!, :gamma])
    param_sum_dict["alpha_median"] = median(mkup_df[!, :alpha])
    param_sum_dict["mkp_median"] = median(mkup_df[!, :mkp])

    param_sum_dict["gamma_p25"] = quantile(mkup_df[!, :gamma], 0.25)
    param_sum_dict["alpha_p25"] = quantile(mkup_df[!, :alpha], 0.25)
    param_sum_dict["mkp_p25"] = quantile(mkup_df[!, :mkp], 0.25)

    param_sum_dict["gamma_p75"] = quantile(mkup_df[!, :gamma], 0.75)
    param_sum_dict["alpha_p75"] = quantile(mkup_df[!, :alpha], 0.75)
    param_sum_dict["mkp_p75"] = quantile(mkup_df[!, :mkp], 0.75)

    param_sum_dict["gamma_trunc_mean"] = mean(trunc_gammas)
    param_sum_dict["alpha_trunc_mean"] = mean(trunc_alphas)
    param_sum_dict["mkp_trunc_mean"] = mean(trunc_mkps)

    param_sum_dict["gamma_trunc_median"] = median(trunc_gammas)
    param_sum_dict["alpha_trunc_median"] = median(trunc_alphas)
    param_sum_dict["mkp_trunc_median"] = median(trunc_mkps)

    param_sum_dict["gamma_trunc_p25"] = quantile(trunc_gammas, 0.25)
    param_sum_dict["alpha_trunc_p25"] = quantile(trunc_alphas, 0.25)
    param_sum_dict["mkp_trunc_p25"] = quantile(trunc_mkps, 0.25)

    param_sum_dict["gamma_trunc_p75"] = quantile(trunc_gammas, 0.75)
    param_sum_dict["alpha_trunc_p75"] = quantile(trunc_alphas, 0.75)
    param_sum_dict["mkp_trunc_p75"] = quantile(trunc_mkps, 0.75)

    param_sum_dict

    num_params = length(keys(bs_sd_dict))
    bs_param_name_arr = Array{Any}(undef, num_params)
    bs_param_est_arr = Array{Any}(undef, num_params)
    bs_mean_arr = Array{Any}(undef, num_params)
    bs_median_arr = Array{Any}(undef, num_params)
    bs_sd_arr = Array{Any}(undef, num_params)
    bs_trunc_sd_arr = Array{Any}(undef, num_params)
    bs_ptiles_arr = Array{Any}(undef, num_params)
    bs_2p5ptiles_arr = Array{Any}(undef, num_params)
    bs_97p5ptiles_arr = Array{Any}(undef, num_params)


    ind = 0
    for param in sort(collect((keys(bs_sd_dict))))
        ind = ind + 1
        bs_param_name_arr[ind] = param
        bs_param_est_arr[ind] = param_sum_dict[param]
        bs_mean_arr[ind] = bs_mean_dict[param]
        bs_median_arr[ind] = bs_median_dict[param]
        bs_sd_arr[ind] = bs_sd_dict[param]
        bs_trunc_sd_arr[ind] = bs_trunc_sd_dict[param]
        bs_2p5ptiles_arr[ind] = bs_lower_95ci_dict[param]
        bs_97p5ptiles_arr[ind] = bs_upper_95ci_dict[param]
    end

    note = "two_stage_bootstrap_errors_paramquantilesum"
    bootstrap_sd_df = DataFrame(parameter=bs_param_name_arr,
        estimate=bs_param_est_arr,
        mean=bs_mean_arr,
        bootstrap_sd=bs_sd_arr,
        trunc_bootstrap_sd=bs_trunc_sd_arr,
        lower_bound=bs_2p5ptiles_arr,
        median=bs_median_arr,
        upper_bound=bs_97p5ptiles_arr
    )

    CSV.write(joinpath(bootstrap_out_dir, "..", "$note" * ".csv"), bootstrap_sd_df)


    bs_alpha_i_dict = Dict{Any,Any}()
    bs_beta_alpha_dict = Dict{Any,Any}()
    bs_beta_gamma_dict = Dict{Any,Any}()
    bs_gamma_i_dict = Dict{Any,Any}()

    bs_gamma_dict = Dict{Any,Any}()
    bs_alpha_dict = Dict{Any,Any}()
    bs_mkup_dict = Dict{Any,Any}()

    for stage1_id in 1:100
        out_dir = joinpath(bootstrap_out_dir, "bs_" * "$stage1_id")

        for stage2_id in 1:100
            println("stage1_id: $stage1_id, stage2_id: $stage2_id")
            alpha_i_df = DataFrame(CSV.File(joinpath(out_dir, "$stage2_id" * "_alpha_i.csv")))
            beta_alpha_df = DataFrame(CSV.File(joinpath(out_dir, "$stage2_id" * "_beta_alpha.csv")))
            beta_gamma_df = DataFrame(CSV.File(joinpath(out_dir, "$stage2_id" * "_beta_gamma.csv")))
            gamma_i_df = DataFrame(CSV.File(joinpath(out_dir, "$stage2_id" * "_gamma_i.csv")))
            mkup_df = DataFrame(CSV.File(joinpath(out_dir, "$stage2_id" * "_mkp_df.csv")))


            ## Final Parameters (by bidder-contract)
            for a in 1:size(mkup_df, 1)

                output_row = mkup_df[a, :]

                bidder_id = output_row[:bidder_id][1]
                contract_no = output_row[:contract_no][1]
                mkp = output_row[:mkp][1]
                alpha = output_row[:alpha][1]
                gamma = output_row[:gamma][1]

                try
                    push!(bs_gamma_dict[contract_no, bidder_id], gamma)
                catch error
                    if isa(error, KeyError)
                        bs_gamma_dict[contract_no, bidder_id] = [gamma]
                    end
                end

                try
                    push!(bs_alpha_dict[contract_no, bidder_id], alpha)
                catch error
                    if isa(error, KeyError)
                        bs_alpha_dict[contract_no, bidder_id] = [alpha]
                    end
                end

                try
                    push!(bs_mkup_dict[contract_no, bidder_id], mkp)
                catch error
                    if isa(error, KeyError)
                        bs_mkup_dict[contract_no, bidder_id] = [mkp]
                    end
                end

            end


            ## Raw Parameters

            ## alpha_i
            for a in 1:nrow(alpha_i_df)
                alpha_row = alpha_i_df[a, :]

                bidder_id = alpha_row[:bidder_id][1]
                alpha_i_est = alpha_row[:alpha_i][1]

                try
                    push!(bs_alpha_i_dict[bidder_id], alpha_i_est)
                catch error
                    if isa(error, KeyError)
                        bs_alpha_i_dict[bidder_id] = [alpha_i_est]
                    end
                end
            end

            ## gamma_i
            for g in 1:nrow(gamma_i_df)
                gamma_row = gamma_i_df[g,:]

                bidder_id = gamma_row[:bidder_id][1]
                gamma_i_est = gamma_row[:gamma_i][1]

                try
                    push!(bs_gamma_i_dict[bidder_id],  gamma_i_est)
                catch error
                    if isa(error, KeyError)
                        bs_gamma_i_dict[bidder_id] = [gamma_i_est]
                    end
                end
            end

            ## Beta_alpha
            for b in 1:nrow(beta_alpha_df)
                beta_row = beta_alpha_df[b, :]

                beta_est = beta_row[:beta][1]

                try
                    push!(bs_beta_alpha_dict[b], beta_est)
                catch error
                    if isa(error, KeyError)
                        bs_beta_alpha_dict[b] = [beta_est]
                    end
                end
            end

            ## Beta_gamma
            for b in 1:nrow(beta_gamma_df)
                beta_row = beta_gamma_df[b, :]

                beta_est = beta_row[:beta][1]

                try
                    push!(bs_beta_gamma_dict[b], beta_est)
                catch error
                    if isa(error, KeyError)
                        bs_beta_gamma_dict[b] = [beta_est]
                    end
                end
            end

        end
    end

    bs_mean_dict = Dict{Any,Any}()
    bs_median_dict = Dict{Any,Any}()
    bs_sd_dict = Dict{Any,Any}()
    bs_ptiles_dict = Dict{Any,Any}()
    bs_trunc_sd_dict = Dict{Any,Any}()
    bs_lower_95ci_dict = Dict{Any,Any}()
    bs_upper_95ci_dict = Dict{Any,Any}()

    trunc95_numdraws = Int(round((100 * 100) / 40))

    enter_param_bootstrap_stats_into_sumdict!(bs_gamma_dict,
        "gamma",
        true,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    enter_param_bootstrap_stats_into_sumdict!(bs_alpha_dict,
        "alpha",
        true,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    enter_param_bootstrap_stats_into_sumdict!(bs_mkup_dict,
        "mkp",
        true,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)


    enter_param_bootstrap_stats_into_sumdict!(bs_gamma_i_dict,
        "gamma_i",
        false,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)


    enter_param_bootstrap_stats_into_sumdict!(bs_alpha_i_dict,
        "alpha_i",
        false,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    enter_param_bootstrap_stats_into_sumdict!(bs_beta_alpha_dict,
        "beta_alpha",
        false,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)

    enter_param_bootstrap_stats_into_sumdict!(bs_beta_gamma_dict,
        "beta_gamma",
        false,
        bs_mean_dict,
        bs_median_dict,
        bs_sd_dict,
        bs_ptiles_dict,
        bs_trunc_sd_dict,
        bs_lower_95ci_dict,
        bs_upper_95ci_dict,
        trunc95_numdraws)


    ## get original parameter estimates ##
    estimate_id = joinpath(bootstrap_out_dir, "bs_0", "0")
    # estimate_id = OG_bootstrap_out_dir * "estimates/individual_gamma_june2022" 

    param_dict = Dict{Any,Any}()

    alpha_i_df = DataFrame(CSV.File(estimate_id * "_alpha_i.csv"))
    beta_alpha_df = DataFrame(CSV.File(estimate_id * "_beta_alpha.csv"))
    beta_gamma_df = DataFrame(CSV.File(estimate_id * "_beta_gamma.csv"))
    gamma_i_df = DataFrame(CSV.File(estimate_id * "_gamma_i.csv"));
    mkup_df = DataFrame(CSV.File(estimate_id * "_mkp_df.csv"));


    ## Final Parameters (Medians)

    med_gamma = median(mkup_df[!, :gamma])
    med_alpha = median(mkup_df[!, :alpha])
    med_mkp = median(mkup_df[!, :mkp])

    param_dict["med_gamma"] = med_gamma
    param_dict["med_alpha"] = med_alpha
    param_dict["med_mkp"] = med_mkp


    # Final Parameters (by bidder-contract) -- need to fix the processing issue for this to work
    for a in 1:nrow(mkup_df)

        output_row = mkup_df[a, :]

        bidder_id = output_row[:bidder_id][1]
        contract_no = output_row[:contract_no][1]
        mkp = output_row[:mkp][1]
        alpha = output_row[:alpha][1]
        gamma = output_row[:gamma][1]

        param_dict["gamma_$contract_no"*"_$bidder_id"] = gamma
        param_dict["alpha_$contract_no"*"_$bidder_id"] = alpha
        param_dict["mkp_$contract_no"*"_$bidder_id"] = mkp

    end


    ## alpha_i
    for a in 1:nrow(alpha_i_df)
        alpha_row = alpha_i_df[a, :]

        bidder_id = alpha_row[:bidder_id][1]
        alpha_i_est = alpha_row[:alpha_i][1]

        param_dict["alpha_i_$bidder_id"] = alpha_i_est

    end

    ## Beta_alpha
    for b in 1:nrow(beta_alpha_df)
        beta_row = beta_alpha_df[b, :]

        beta_est = beta_row[:beta][1]

        param_dict["beta_alpha_$b"] = beta_est

    end

    ## Beta_gamma
    for b in 1:nrow(beta_gamma_df)
        beta_row = beta_gamma_df[b, :]

        beta_est = beta_row[:beta][1]
        param_dict["beta_gamma_$b"] = beta_est

    end

    ## gammaa_i
    for a in 1:size(alpha_i_df, 1)
        gamma_row = gamma_i_df[a, :]

        bidder_id = gamma_row[:bidder_id][1]
        gamma_i_est = gamma_row[:gamma_i][1]

        param_dict["gamma_i_$bidder_id"] = gamma_i_est

    end

    num_params = length(keys(bs_sd_dict))
    bs_param_name_arr = Array{Any}(undef, num_params)
    bs_mean_arr = Array{Any}(undef, num_params)
    bs_median_arr = Array{Any}(undef, num_params)
    bs_sd_arr = Array{Any}(undef, num_params)
    bs_trunc_sd_arr = Array{Any}(undef, num_params)
    bs_ptiles_arr = Array{Any}(undef, num_params)
    bs_2p5ptiles_arr = Array{Any}(undef, num_params)
    bs_97p5ptiles_arr = Array{Any}(undef, num_params)


    ind = 0
    for param in sort(collect((keys(bs_sd_dict))))
        ind = ind + 1
        bs_param_name_arr[ind] = param
        bs_mean_arr[ind] = bs_mean_dict[param]
        bs_median_arr[ind] = bs_median_dict[param]
        bs_sd_arr[ind] = bs_sd_dict[param]
        bs_trunc_sd_arr[ind] = bs_trunc_sd_dict[param]
        bs_2p5ptiles_arr[ind] = bs_lower_95ci_dict[param]
        bs_97p5ptiles_arr[ind] = bs_upper_95ci_dict[param]
    end

    note = "two_stage_bootstrap_errors"
    bootstrap_sd_df = DataFrame(parameter=bs_param_name_arr,
        mean=bs_mean_arr,
        bootstrap_sd=bs_sd_arr,
        trunc_bootstrap_sd=bs_trunc_sd_arr,
        lower_bound=bs_2p5ptiles_arr,
        median=bs_median_arr,
        upper_bound=bs_97p5ptiles_arr
    )

    CSV.write(joinpath(bootstrap_out_dir, "..", "$note" * ".csv"), bootstrap_sd_df)

    num_params = length(keys(bs_sd_dict))
    bs_param_name_arr = Array{Any}(undef, num_params)
    bs_param_est_arr = Array{Any}(undef, num_params)
    bs_mean_arr = Array{Any}(undef, num_params)
    bs_median_arr = Array{Any}(undef, num_params)
    bs_sd_arr = Array{Any}(undef, num_params)
    bs_trunc_sd_arr = Array{Any}(undef, num_params)
    bs_2p5ptiles_arr = Array{Any}(undef, num_params)
    bs_97p5ptiles_arr = Array{Any}(undef, num_params)


    ind = 0
    for param in sort(collect((keys(bs_sd_dict))))
        ind = ind + 1
        bs_param_name_arr[ind] = param
        bs_param_est_arr[ind] = param_dict[param]
        bs_mean_arr[ind] = bs_mean_dict[param]
        bs_median_arr[ind] = bs_median_dict[param]
        bs_sd_arr[ind] = bs_sd_dict[param]
        bs_trunc_sd_arr[ind] = bs_trunc_sd_dict[param]
        #     bs_ptiles_arr[ind] = (bs_ptiles_dict[param][1])
        bs_2p5ptiles_arr[ind] = bs_lower_95ci_dict[param]
        bs_97p5ptiles_arr[ind] = bs_upper_95ci_dict[param]
    end


    note = "two_stage_param_bootstrap_errors"

    bootstrap_sd_df = DataFrame(parameter=bs_param_name_arr,
        estimate=bs_param_est_arr,
        mean=bs_mean_arr,
        bootstrap_sd=bs_sd_arr,
        trunc_bootstrap_sd=bs_trunc_sd_arr,
        lower_bound=bs_2p5ptiles_arr,
        median=bs_median_arr,
        upper_bound=bs_97p5ptiles_arr
    )

    CSV.write(joinpath(bootstrap_out_dir, "..", "$note" * ".csv"), bootstrap_sd_df)

    note = "second_stage_estimates_summary"

    bootstrap_sd_df = DataFrame(
        parameter=bs_param_name_arr,
        estimate=bs_param_est_arr,
        mean=bs_mean_arr,
        bootstrap_sd=bs_sd_arr,
        trunc_bootstrap_sd=bs_trunc_sd_arr,
        lower_bound=bs_2p5ptiles_arr,
        median=bs_median_arr,
        upper_bound=bs_97p5ptiles_arr
    )

    CSV.write(joinpath(bootstrap_out_dir, "..", "$note" * ".csv"), bootstrap_sd_df)


    # Copy the 0_* files to the step 4 output
    base_dir = joinpath(bootstrap_out_dir, "bs_0", "0_")

    suffixes = [
        "alpha_i",
        "beta_alpha",
        "beta_gamma",
        "gamma_i",
        "mkp_df",
    ]
    for suffix in suffixes
        src = joinpath(base_dir * "$suffix.csv")
        dst = joinpath(bootstrap_out_dir, "..", "second_stage_estimates_$suffix.csv")
        cp(src, dst, force=true)
    end
end

go()

