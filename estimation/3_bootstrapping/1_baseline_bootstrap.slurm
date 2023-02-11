#!/bin/bash

# This is a job array to run a bootstrap run for 100 random draws from the posterior 1st stage distribution
# Submit script: array=1-100 to process all 100 1st stage bootstrap draws
# This script will fill the appropriate folders specified in the file
# Note that each bootstrap run does 100 boostrap estimations and so may still take hours

#SBATCH -J bs0
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -t 2-00:00:00
##SBATCH --mem=4gb
#SBATCH -o logs/julia-%A-0.out
##SBATCH --mail-type=ALL

# Load julia
module load julia/1.7.3
module load knitro/12.1.1

# Instantiate Julia environment
srun julia --project -e 'using Pkg; Pkg.instantiate()'

# Make a log folder if it doesn't exist
mkdir -p logs

# Run julia with a command line arg
srun julia --project CARA_bootstrap_cluster.jl 0 > logs/bootstrap_run_0.txt
