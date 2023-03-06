# Replication

The files in this directory contain all data & code relevant for replication of the results in "Scaling Auctions as Insurance: A Case Study in Infrastructure Procurement" by Valentin Bolotnyy and Shoshana Vasserman.

## Directory structure

- `estimation` contains the code used to estimate parameters and counterfactuals.
- `presentation` contains all code used to reshape and present the final results of the paper.
- `data` contains the relevant data used by the code in `estimation` and `presentation`.

## Version information

The code employed for this paper is in [R](https://www.r-project.org/), [Julia](https://julialang.org/), and [Stan](https://mc-stan.org/). 

For the R code, we use R version `4.0.2`. See the file `r_info.txt` for exact details on the R environment. 

The Julia version is `1.7.3` but should function reasonably well on the most recent long-term support release (`1.6.7` as of December 19th, 2022). We provide the package environments `Manifest.toml` and `Package.toml` for convenience. To instantiate the package environment (download all the packages in the `Manifest.toml`), load Julia from the directory where the `Manifest.toml` and `Package.toml` files are, and enter the following into the REPL:

```julia
import Pkg
Pkg.activate(".")
Pkg.instantiate()
```

The Stan version used is `2.21.0`.

# Replication guide

The replication package is broken into two components, `estimation` and `presentation`. `estimation` focuses on the generation of the key parameters of interest in the paper, such as risk aversion, entry costs, and predicted auction costs under counterfactual conditions.

`presentation` focuses on generating plots and tables of the results. The code in `presentation` is designed to be run after the code in `estimation` has been run. The code in `presentation` will load the relevant data from the `data` directory.

Please note that the code in `estimation` may take _significant_ computational resources to fully replicate. The code in `presentation` will run on a standard desktop or laptop computer in under a minute total, while `estimation` can take weeks to complete given fully parallelized compute resources. For specific queries regarding the replication package that are not answered in this document, please contact the authors.

## Estimation

### Step 1: Run model predicted quantities

Step 1 produces model-estimated quantities used in the construction of projects.

**Language**. R.

**Run time**. Approximately 80 minutes on a desktop with an Intel(R) Core(TM) i9-9900K CPU @ 3.60GHz. 

**Code location**: 

- Primary: `estimation/1_estimate_first_stage.R`

**Input files**: 

- `data/estimation_step_1_qa_input_data.rdata`

**Output files**:

- `data/estimation_step1_output_fullfit.rdata`
- `data/estimation_step1_output_minfit.rdata`

### Step 2: Prepare bootstrap samples.

This script produces 100 directories containing bootstrapped samples. Each new bootstrap sample is placed in its own directory with the format `data/estimation_step2_inputs/bs_$BOOTSTRAP_NUMBER` where `$BOOTSTRAP_NUMBER` represents the bootstrap sample identifier. Each folder contains 1,752 files used in the GMM estimation.

**Language**. R.

**Run time**. 5-30 minutes on a desktop with an Intel(R) Core(TM) i9-9900K CPU @ 3.60GHz. 

**Code location**: 

- Primary: `estimation/2_prep_second_stage.R`

**Input files**: 

- `data/estimation_step1_output_fullfit.rdata`

**Output files**:

- `data/estimation_step2_inputs/`

### Step 3: Run bootstrapping.

The code in this section uses the bootstrapped samples from Step 2 to produced bootstrapped GMM estimates. The code in this section can be run in two ways, depending on the computation environment available. Please use the Julia environment specified in `estimation/3_bootstrapping/`. You can access this environment by navigating to `estimation/3_bootstrapping/` in your shell and calling `julia --project`, or by activating the environment with `import Pkg; Pkg.activate("estimation/3_bootstrapping/")`.

The script `estimation/3_bootstrapping/CARA_bootstrap_local.jl` is intended to be run on a single machine, such as a desktop or laptop. This is the slowest option and may take several days to do in serial. The local script supports multithreading, but note that memory needs are quite large. To introduce additional threads into the thread pool, start Julia with extra threads using `julia --threads=4`.

The alternative script `estimation/3_bootstrapping/CARA_bootstrap_cluster.jl` was written to be dispatched onto a slurm-compatible compute cluster, such as [Stanford's Yen servers](https://rcpedia.stanford.edu/yen/). This script must be adapted to fit your local needs -- in particular, please modify line 21, which contains the KNITRO license server location, if available (Line 21). Please also see the slurm script `bootstrap.slurm`, which details example settings used to parallelize the GMM bootstrap across slurm nodes.

The solver supports two optimizers, KNITRO (commercial) and Ipopt (free). To use KNITRO if it is available, please set the constant `USE_KNITRO=true` in your script (either `CARA_bootstrap_cluster.jl` or `CARA_bootstrap_local.jl`). The estimates in the paper were produced using KNITRO version `12.1.1`.

**Language**. Julia.

**Run time**. 1-7 days using the local file, 20 hours on a parallelized slurm cluster (depending on resources available). Less than ten minutes if producing only primary estimates.

**Code location**: 

- Primary estimates: `estimation/3_bootstrapping/produce-summary-stats.jl`
- Bootstap estimation:
    - Local: `estimation/3_bootstrapping/CARA_bootstrap_local.jl`, or
    - Cluster: `estimation/3_bootstrapping/CARA_bootstrap_cluster.jl`
- Bootstrap consolidation:
    - Bootstrapped estimates: `estimation/3_bootstrapping/process-bootstrap-results.jl`

**Input files**: 

- `data/estimation_step1_output_fullfit.rdata`
- `data/estimation_step2_inputs/`

**Output files**:

- Primary: `data/estimation_step3/second_stage_estimates_summary.csv`
- `data/estimation_step3/two_stage_bootstrap_errors.csv`
- `data/estimation_step3/two_stage_bootstrap_errors_paramquantilesum.csv`
- `data/estimation_step3/two_stage_bootstrap_errors.csv`
- `data/estimation_step3/two_stage_param_bootstrap_errors.csv`
- `data/estimation_step3/second_stage_estimates_alpha_i.csv`
- `data/estimation_step3/second_stage_estimates_beta_alpha.csv`
- `data/estimation_step3/second_stage_estimates_beta_gamma.csv`
- `data/estimation_step3/second_stage_estimates_gamma_i.csv`
- `data/estimation_step3/second_stage_estimates_mkp_df.csv`
- `data/estimation_step3/second_stage_estimates_bid_fit.csv`

### Step 4: Entry preprocessing

This step consists of three scripts that:

1. Assign auctions into bins.
2. Estimate the mean, variance, and truncation parameters of the distribution of bidder types (`alpha`).
3. Post-process the data for the entry model.

The second step estimates a Stan model, located in `estimation/4_preprocess/4_alpha_lgnorm_reg_model_binomial_joint.stan`.

**Language**. R, Stan.

**Run time**. 10-30 minutes.

**Code location**: 

- `estimation/4_entry_preprocess/4a_create_cf_input_data_step1_projbins.R`
- `estimation/4_entry_preprocess/4b_create_cf_input_data_step2_fitalphamodel.R`
- `estimation/4_entry_preprocess/4c_create_cf_input_data_step3_export.R`

**Input files**:

- `data/estimation_step1_output_fullfit.rdata`
- `data/estimation_step3/second_stage_estimates_summary.csv`

**Output files**:

- `data/estimation_step4_cf_inputs/cfdata.csv`
- `data/estimation_step4_cf_inputs/alphas_by_auction_with_features.csv`
- `data/estimation_step4_cf_inputs/potential_bidders_by_contract.csv`

### Step 5: Entry

Step 5 produces auction outcomes across a range of grid points for entry cost and the proportion of extra work orders expected to be earned. Each auction and quantity type (model estimate vs. actual) generates a file containing all the information about the grid parameters lambda (expected EWO) and K (entry cost), and various moments at the specified grid point.

This script is intended to be parallelized across a slurm cluster, as computing it on a single machine can take more than a week depending on available resources. 

The bash script `5-run-entry.bash` is intended to be run on a slurm cluster and will parallelize the Julia script `5-entry-calibration.jl` across a range of nodes. The Julia script `a-consolidate-entry-files.jl` will consolidate the results of the parallelized runs into a single file.

**Language**. Julia.

**Run time**. 1-10 days, depending on resources available.

**Code location**: 

- Part 1, estimation
    - `estimation/5_entry/5-run-entry.bash`
    - `estimation/5_entry/5-entry-calibration.jl`
- Part 2, consolidation
    - `estimation/5_entry/5a-consolidate-entry-files.jl`

**Input files**: 

- `data/estimation_step4_cf_inputs/` (uses all files in this directory)

**Output files**:

- `data/estimation_step5_outputs/` (multiple files including `step5_entry_calibration_output.csv`)

### Step 6: Entry parameter selection

Step 6 selects the entry parameters (entry cost and expected EWO proportion earned) that minimize the difference between actual and simulated auction outcomes. The parameters are chosen on a per-bin basis as the parameters that minimize the total square root of the entry norm and the CDF norm within a bin.

**Language**. R.

**Run time**. 10 seconds.

**Code location**: 

- `estimation/6-get_calibrated_entry_params.R`

**Input files**: 

- `data/estimation_step5_outputs/step5_entry_calibration_output.csv`

**Output files**:

- `data/estimation_step6_precf/calibrated_entry_params_binlevel.csv`

**Tables**:

- Table 5a: `outputs/cf_results/table5a.tex`
- Table 5b: `outputs/cf_results/table5b.tex`

### Step 7: Counterfactual estimation

Step 7 estimates the counterfactual auction outcomes for the selected entry parameters. The script `run-counterfactuals.bash` is intended to be run on a slurm cluster and will parallelize the estimation across a range of nodes. The script `consolidate-counterfactuals.jl` will consolidate the results of the parallelized runs into a single file.

**Language**. Julia.

**Run time**. 1-10 days, depending on resources available.

**Code location**: 

- Estimation: `estimation/7_counterfactual/7-counterfactuals.jl`
- Bash script: `estimation/7_counterfactual/run-counterfactuals.bash`
- Consolidation: `estimation/7_counterfactual/consolidate-counterfactuals.jl`

**Input files**: 

- `data/estimation_step4_cf_inputs/` (uses all files in this directory)
- `data/estimation_step5_outputs/step5_entry_calibration_output.csv`
- `data/estimation_step6_precf/calibrated_entry_params_binlevel.csv`

**Output files**:

- `data/estimation_step7_counterfactuals/` (multiple files including `calibrated_entry_params_binlevel/aggregate_results.csv` and `calibrated_entry_params_binlevel/detailed_results.csv`)

### Step 8: Moral hazard

Step 8 re-estimates the entry model permitting moral hazard by bidders. The script `8-hazard.jl` will estimate the model using either Gurobi or Ipopt. Since each bidder must solve a mixed integer program, the moral hazard model is significantly more computationally intensive than the baseline model, and may take significant resources to run. 

To use Gurobi, you must first install Gurobi and obtain a license. Starting Julia with the environment variable `JLGUROBI=true` will enable the Gurobi solver. If Gurobi is not used, the script will default to the open-source solver Ipopt. Ipopt is not recommended for the replication package as Gurobi is significantly faster.

**Language**. Julia.

**Run time**. 4-15 days, depending on resources available.

**Code location**: 

- `estimation/8_moral_hazard/8-hazard.jl`
- `estimation/8_moral_hazard/consolidate-mh.jl`
- `estimation/8_moral_hazard/run-mh.bash`

**Input files**: 

- `data/estimation_step4_cf_inputs/` (uses all files in this directory)
- `data/estimation_step5_outputs/step5_entry_calibration_output.csv`
- `data/estimation_step6_precf/calibrated_entry_params_binlevel.csv`

**Output files**:

- `data/estimation_step8_moralhazard/` (multiple files including `mh_results_nocap.csv`)

## Presentation

The following scripts generate the tables and figures in the paper.

Several of the steps use an R package developed to plot binscatters. The relevant source code is in `presentation/binscattr`. All code that uses `binscattr` will attempt to install `binscattr` to a local library (`presentation/.library`) and should leave the user's existing library untouched. No action is required on behalf of the user.

### Step 1: Descriptive statistics

The code in the script `presentation/1_data_description.R` generates the descriptive statistics in the paper. The script will generate a number of figures and tables. Outputs are stores in the directory `outputs/data_description/`.

**Language**. R.

**Run time**. 3-5 seconds.

**Code location**: 

- `presentation/1_data_description.R`

**Input files**: 

- `data/estimation_step1_output_minfit.rdata`

**Output files**:

- `outputs/data_description/`

**Tables**:

- Table 1: `outputs/data_description/table1.tex`
- Table 2: `outputs/data_description/table2.tex`

**Figures**:

- Figure 1: `outputs/data_description/figure1.jpg`
- Figure 2a: `outputs/data_description/figure2a.jpg`
- Figure 2b: `outputs/data_description/figure2b.jpg`

### Step 2: Reduced form

Step 2 produces plots and figures that summarize the results of the reduced form estimates. The script `presentation/2_reduced_form.R` will generate a number of figures and tables. Outputs are stored in the directory `outputs/reduced_form/`.

**Language**. R.

**Run time**. 5-10 seconds.

**Code location**: 

- `presentation/2_reduced_form.R`

**Input files**: 

- `data/estimation_step1_output_minfit.rdata`

**Output files**:

- `outputs/reduced_form/`

**Figures**:

- Figure 3a: `outputs/reduced_form/fig3a.jpg`
- Figure 3b: `outputs/reduced_form/fig3b.jpg`
- Figure 4a: `outputs/reduced_form/fig4a.jpg`
- Figure 4b: `outputs/reduced_form/fig4b.jpg`
- Figure 10a: `outputs/reduced_form/app_fig10a.jpg`
- Figure 10b: `outputs/reduced_form/app_fig10b.jpg`
- Figure 11a: `outputs/reduced_form/app_fig11a.jpg`
- Figure 11b: `outputs/reduced_form/app_fig11b.jpg`
- Figure 13: `outputs/reduced_form/app_fig14.jpg`
- Figure 14: `outputs/reduced_form/app_fig15.jpg`

### Step 3: Estimation results

Step 3 produces figures and tables that summarize estimates for bidder efficiency (`alpha`), risk aversion (`gamma`), and markups. The script `presentation/3_estimation_results.R` will generate a number of figures and tables. Outputs are stored in the directory `outputs/estimation_results/`.

**Language**. R.

**Run time**. 1-2 seconds.

**Code location**: 

- `presentation/3_estimation_results.R`

**Input files**: 

- `data/estimation_step1_output_minfit.rdata`
- `data/estimation_step3/second_stage_estimates_summary.csv`

**Output files**:

- `outputs/estimation_results/`

**Tables**:

- Table 3a: `outputs/estimation_results/table3a.tex`
- Table 3b: `outputs/estimation_results/table3b.tex`
- Table 4a: `outputs/estimation_results/table4a.tex`
- Table 4b: `outputs/estimation_results/table4b.tex`
- Table 11: `outputs/estimation_results/app_table11.tex`

**Figures**:

- Figure 5a: `outputs/estimation_results/fig5a.jpg`
- Figure 5b: `outputs/estimation_results/fig5b.jpg`
- Figure 9: `outputs/estimation_results/app_fig9.jpg`

### Step 4: Counterfactuals

Step 4 summarizes the counterfactual results. The script `presentation/4_counterfactual_results.R` will generate a number of figures and tables. Outputs are stored in the directory `outputs/cf_results/`.

**Language**. R.

**Run time**. 1-2 seconds.

**Code location**: 

- `presentation/4_counterfactual_results.R`

**Input files**: 

- `data/estimation_step7_counterfactuals/calibrated_entry_params_binlevel/aggregate_results.csv`
- `data/estimation_step7_counterfactuals/calibrated_entry_params_binlevel/detailed_results.csv`
- `data/estimation_step4_cf_inputs/alpha_gamma_distribution.csv`

**Output files**:

- `outputs/cf_results/`

**Tables**:

- Table 11: `outputs/cf_results/app_table12.tex`
- Table 12a: `outputs/cf_results/app_table13a.tex`
- Table 12b: `outputs/cf_results/app_table13b.tex`

### Step 5: Moral hazard

Step 5 summarizes the moral hazard results. The script `presentation/5_moral_hazard_results.R` will generate a number of figures and tables. Outputs are stored in the directory `outputs/mh_results/`.

**Language**. R.

**Run time**. 1-2 seconds.

**Code location**: 

- `presentation/5_moral_hazard_results.R`

**Input files**: 

- `data/estimation_step8_moralhazard/mh_results_nocap.csv`
- `data/estimation_step4_cf_inputs/alpha_gamma_distribution.csv`

**Output files**:

- `outputs/mh_results/`

**Tables**:

- Table 6: `outputs/mh_results/app_table6.tex`

### Appendix: GMM & Quantities

The script `presentation/N_appendix_estimation_results.R` produces additional figures and plots used in the appendix, primarily on actual vs. predicted quantities. Outputs are stored in the directory `outputs/estimation_results/`.

**Language**. R.

**Run time**. 30 seconds.

**Code location**: 

- `presentation/N_appendix_estimation_results.R`

**Input files**: 

- `data/estimation_step1_output_minfit.rdata`
- `data/estimation_step3/second_stage_estimates_summary.csv`
- `data/estimation_step3/second_stage_estimates_bid_fit.csv`
- `data/estimation_step3/two_stage_bootstrap_errors_paramquantilesum.csv`
- `data/estimation_step8_moralhazard/mh_results_nocap.csv`

**Output files**:

- `outputs/estimation_results/`

**Tables**:

- Table 8: `outputs/estimation_results/app_table8.tex`
- Table 9: `outputs/estimation_results/app_table9.tex`
- Table 10: `outputs/estimation_results/app_table10.tex`

**Figures**:

- Figure 6: `outputs/estimation_results/app_fig6.jpg`
- Figure 7: `outputs/estimation_results/app_fig7.jpg`
- Figure 8b: `outputs/estimation_results/app_fig8a.jpg`
- Figure 8b: `outputs/estimation_results/app_fig8b.jpg`

### Appendix: Quantity model

The script `presentation/N_appendix_qamodel_estimate_table.R` summarizes the parameters that govern model-predicted quantities (quantities denoted in the paper as $q^a_t$). 

The two parameters of interested are written in the paper as $\beta_q$ and $\beta_\sigma$. The code that estimates the parameters maps these two parameters to the following:

- $\beta_q$: `beta`
- $\beta_\sigma$: `gamma`

To produce the relevant posterior summary statistics, we used `shinystan`, a web application for exploring Stan output. To replicate the table: 

1. Please run the script `presentation/N_appendix_qamodel_estimate_table.R` to start `shinystan`.
2. In the browser window that opens, click "Estimate".
3. In the "Estimate" tab, click "Generate LaTeX table".
4. In the "Parameters" text window, select "ALL gamma" and "ALL beta".
5. Copy the LaTeX table to your document.

**Language**. R.

**Run time**. 5 minutes.

**Code location**: 

- `presentation/N_appendix_qamodel_estimate_table.R`

**Input files**: 

- `data/estimation_step1_output_fullfit.rdata`

**Tables**:

- Table 7
