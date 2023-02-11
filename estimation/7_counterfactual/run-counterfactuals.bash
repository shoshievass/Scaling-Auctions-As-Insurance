#!/bin/bash
#
#SBATCH --job-name=counterfactuals
#
#SBATCH --time=2-0 # For short partition
#SBATCH --ntasks=1
##SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=8G
#SBATCH --output=/scratch/groups/svass/ScalingAuctions/slurm-logs/%A-%a.out
#SBATCH --mail-type=FAIL
#SBATCH --array=1-430
#SBATCH --threads-per-core=1

module load julia/1.7.3
JULIA_NUM_THREADS=auto julia 7-counterfactuals.jl --threads=auto
#JULIA_NUM_THREADS=auto julia --project entry-test.jl
