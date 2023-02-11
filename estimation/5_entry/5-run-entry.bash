#!/bin/bash
#
#SBATCH --job-name=entry
#
##SBATCH --time=7-0
#SBATCH --time=0-24 # For short partition
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=32G
#SBATCH --output=/zfs/projects/faculty/svass-procurement/ScalingResults/slurm-logs/%A-%a.out
#SBATCH --mail-type=ALL
#SBATCH --array=1-430
##SBATCH --qos=long

module load julia/1.7.3
export GROUP_SCRATCH=/zfs/projects/faculty/svass-procurement/ScalingResults/
USE_ACTUAL=true  julia 5-entry-calibration.jl --threads=auto
USE_ACTUAL=false julia 5-entry-calibration.jl --threads=auto
