#########
# Inputs: ARGS[1] takes the array index from slurm
# Outputs: For qa-model draw # [ARGS[1]], run estimation for each of 100 draws of auctions defined by bootstrap_auction_samples.csv
########
import Pkg; Pkg.activate(@__DIR__)

using JuMP
using DataFrames, CSV
using Dates
using StatsBase
using Random
using Ipopt, KNITRO

# Set this to true to use KNITRO, if it is available.
# KNITRO is significantly faster than Ipopt.
const USE_KNITRO=false

today = Dates.today()

project_dir = @__DIR__
estimation_results_dir = joinpath(project_dir, "..", "..", "data", "estimation_step3")

include(joinpath(project_dir, "CARA_Estimation_bootstrap_fns.jl"))
include(joinpath(project_dir, "CARA_Estimation_Structs.jl"))

function bootstrap(index)
    # Set up paths
    julia_data_dir = joinpath(
        estimation_results_dir, 
        "..", 
        "estimation_step2_inputs", 
        "bootstrap",
        "bs_" * "$index/"
    )
    bootstrap_out_dir = joinpath(estimation_results_dir, "second_stage_estimates_bootstrap_raw_results/")

    out_dir = bootstrap_out_dir * "bs_" * "$index" * "/"
    isdir(bootstrap_out_dir) || mkpath(bootstrap_out_dir)
    isdir(out_dir) || mkpath(out_dir)

    raw_contract_list = [ 30163, 30230, 31050, 31063, 31082, 31084, 31172, 31200, 31267, 31292, 31296, 31348, 31353, 31356, 31358, 31359, 32029, 32045, 32052, 32075, 32080, 32087, 32096, 32109, 32126, 32130, 32160, 32189, 32192, 32200, 32220, 32253, 32264, 32270, 32284, 32286, 32292, 32301, 33000, 33025, 33064, 33065, 33067, 33068, 33122, 33126, 33144, 33152, 33153, 33161, 33199, 33201, 33203, 33237, 33238, 33251, 33253, 33260, 33272, 33276, 33278, 33282, 33305, 33316, 33319, 34004, 34034, 34036, 34043, 34056, 34059, 34449, 34450, 34451, 34452, 34460, 34485, 34486, 34489, 34508, 34510, 34520, 34531, 34548, 34569, 34584, 34585, 34594, 34600, 34612, 34636, 34644, 34653, 34668, 34701, 35032, 35035, 35036, 35657, 35842, 36127, 36912, 37671, 38095, 38122, 38638, 38807, 38900, 39036, 39147, 39651, 39741, 39742, 39899, 39932, 39967, 41052, 41064, 41067, 41198, 41586, 41852, 41863, 42088, 42097, 42177, 42308, 42404, 42405, 42409, 42616, 42947, 43123, 43326, 43332, 43482, 43627, 43675, 43702, 43781, 43821, 44007, 44013, 44196, 44339, 44341, 44385, 44828, 45142, 45263, 45409, 45411, 45509, 45564, 45713, 48016, 48023, 48168, 48688, 49428, 49429, 49437, 49586, 51216, 51231, 51326, 51392, 51409, 51588, 51612, 51676, 51773, 51785, 51847, 52030, 52128, 52511, 52513, 52649, 52698, 52805, 52871, 53243, 53270, 53347, 53408, 53414, 53472, 53482, 53491, 53517, 53521, 53558, 53567, 53865, 53915, 53932, 53966, 54020, 54045, 54172, 54173, 54244, 54278, 54336, 54360, 54559, 54707, 54747, 54992, 55026, 55339, 55488, 55538, 55679, 55843, 55855, 55981, 56067, 56216, 56387, 56482, 56515, 56557, 56580, 56608, 56636, 56745, 56746, 56748, 56750, 56779, 56836, 56846, 56859, 56993, 56995, 57045, 57162, 57182, 57227, 57275, 57342, 57343, 57344, 57381, 57419, 57452, 57455, 57636, 57640, 57641, 57664, 57675, 57676, 57757, 57759, 57804, 57889, 57962, 57963, 58005, 58007, 58045, 58047, 58085, 58134, 58658, 58901, 59049, 59130, 59245, 59675, 59676, 59677, 59772, 59773, 59934, 59935, 59936, 60073, 60075, 60150, 60195, 60198, 60310, 60311, 60370, 60371, 60374, 60857, 61463, 61793, 61912, 61916, 61941, 62025, 62564, 62672, 62908, 62909, 63233, 63234, 63235, 63236, 63317, 63700, 63701, 63793, 63796, 63977, 63978, 64519, 64522, 64843, 64848, 65190, 65274, 65281, 65441, 65602, 65605, 65709, 65714, 65827, 65951, 65952, 65956, 66086, 66090, 66091, 66333, 66336, 66457, 66460, 66461, 66466, 66564, 66755, 66834, 66838, 66843, 67019, 67020, 67022, 67059, 67099, 67215, 67220, 67308, 67311, 67313, 67314, 67528, 67530, 67664, 67756, 67757, 67759, 67813, 67817, 67947, 68026, 68135, 68145, 68155, 68164, 68166, 68171, 68186, 68187, 68209, 68223, 68305, 68373, 68530, 68531, 69940, 70213, 70380, 70720, 70723, 70726, 70936, 71085, 71090, 71092, 71095, 71101, 71225, 71228, 72260, 72306, 72313, 72315, 72560, 72562, 73931, 74405, 75231, 75232, 75236, 75351, 75353, 75356, 75581, 75582, 75584, 75704, 75772, 76248, 76401, 76477, 76731, 76936, 76939, 78187, 78541, 79145, 79291, 79293, 79294, 79399, 79512, 79519, 79598, 79732, 79737, 79743, 79871, 79881, 80194, 84362, 99030, 99175, 99179, 99185, 99242]

    println(raw_contract_list)
    bs_df = getBootstrapContractDf(100, raw_contract_list)
    CSV.write(bootstrap_out_dir * "bootstrap_auction_samples.csv", bs_df)

    # Run actual estimation
    run_bootstrap_projGamma(raw_contract_list, 0, julia_data_dir, out_dir)

    ## Run bootstrap estimation test
    bs_df = DataFrame(CSV.File(joinpath(estimation_results_dir, "second_stage_estimates_bootstrap_raw_results/bootstrap_auction_samples.csv")));

    for bs_ind in 1:100
        sample_list = parseContractListFromDf(bs_df[!,:bs_list][bs_ind])
        run_bootstrap_projGamma(sample_list, bs_ind, julia_data_dir, out_dir)
    end
end

function parseContractListFromDf(list_string)
    clean_list_string = list_string[2:(end-1)];
    split_list = split(clean_list_string, ", ")

    parsed_list = [parse(Int, s) for s in split_list]

    return(parsed_list)
end


for sample_id in 0:100
    result = bootstrap(sample_id)
end

