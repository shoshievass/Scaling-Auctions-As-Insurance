##==================================================
##
## Script name: Estimate Quantity Model 
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script: Estimate a model of beliefs over
##                    ex-post quantities with Hamiltonian Monte Carlo
##
## Email: svass@stanford.edu
##
## Input:  data/estimation_step_1_qa_input_data.rdata
##
## Output: data/estimation_step1_output_fullfit.rdata
##         data/estimation_step1_output_minfit.rdata
##
##==================================================

# Packages
library(tidyverse); library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("data/estimation_step_1_qa_input_data.rdata")
X_q <- as.matrix(cbind(demo_project_item_features_df[1:15], demo_project_item_df$lump_item, demo_project_item_df$share_0, demo_project_item_df$top_skew_item))
X_q <- scale(X_q)

raw_data_list_qa <-
  list(
    N = length(unique_project_item_ids),
    P = as.integer(ncol(X_q)),
    X = X_q,
    q_at = demo_project_item_df$q_at,
    q_ot = demo_project_item_df$q_ot,
    state = ifelse(demo_project_item_df$q_at == demo_project_item_df$q_ot, 2, ifelse(demo_project_item_df$q_at == 0, 1, 3))
  )

qa_model <- stan_model("estimation/auxiliary/1_estimate_first_stage_model.stan")

qa_fit <- sampling(qa_model, data = raw_data_list_qa, cores = 4, seed = 938086356)

## Save full model fit object -- needed for bootstrapped estimates ##
save.image(file = paste0("data/estimation_step1_output_fullfit.rdata"))

qa_model_raw_fit <- get_posterior_mean(qa_fit, pars = "q_at_model")[, 5]
sigma_t_raw_fit <- get_posterior_mean(qa_fit, pars = "sigma_t")[, 5]

qa_model_fit <- qa_model_raw_fit * demo_project_item_df$q_ot
sigma_t_fit <- sigma_t_raw_fit  * demo_project_item_df$q_ot
rm(qa_fit)

## Save light version of model fit -- for everything other than bootstrapped estimates ##
save.image(file = paste0("data/estimation_step1_output_minfit.rdata"))
