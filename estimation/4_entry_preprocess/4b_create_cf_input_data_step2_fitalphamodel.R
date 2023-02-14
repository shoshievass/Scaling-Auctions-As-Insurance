##==================================================
##
## Script name: 4b_create_cf_input_data_step2_fitalphamodel
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script: Fit lognormal model to estimated alpha distribution
##
## Input:  data/estimation_step4_cf_inputs/potential_bidders_by_contract.csv
##         data/estimation_step1_output_minfit.rdata
##         data/estimation_step3/second_stage_estimates_summary.csv
##
## Output: data/estimation_step4_cf_inputs/alphas_by_auction_with_features.csv
##
##
library(fixest)
library(tidyverse)
library(recipes)
library(rstan)
rstan_options(auto_write = TRUE)

## directory names for the counterfactuals pertaining to the same 2nd stage estimates
cf_inputs_dir = "data/estimation_step4_cf_inputs/"
alpha_model_df <- read_csv(paste0(cf_inputs_dir,"alphas_by_auction_with_features.csv"))

potential_bidders_filename <- "potential_bidders_by_contract.csv"

## get potential bidders ##
potential_bidders <- read_csv(paste0(cf_inputs_dir,potential_bidders_filename)) %>%
  select(contract_no, max_num_bidders__seen, num_bidders_seen) %>% ## can use max_num_bidders__unique here instead
  rename(
    max_num_bidders = max_num_bidders__seen,
    num_bids = num_bidders_seen
  )

potential_bidder_bin_features <- read_csv(paste0(cf_inputs_dir,potential_bidders_filename)) %>%
  select(
    contract_no, 
    potential_bidder_bin_id,
    proj_manager_id,
    engineer_id,
    designer_id
  )
  
alpha_model_df <- alpha_model_df %>%
  left_join(potential_bidders %>% select(contract_no, max_num_bidders, num_bids))


## Clean up X vars.
alpha_model_for_reg <- alpha_model_df %>%
  select(-contract_no, -project_id_sequential, -max_num_bidders, -num_bids, -bridge) %>%
  select(-ends_with("id"), -bid_open_year_rounded, -district, -bid_open_year) ##To make life easier for now, let's actually remove all the extra factors

alpha_model <- recipe(alpha ~ ., data = alpha_model_for_reg)

alpha_model <- alpha_model %>%
  step_scale(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_lincomb(all_numeric_predictors()) %>%
  prep()

alpha_model_input <- bake(alpha_model, alpha_model_for_reg) %>%
  relocate(alpha)

alpha_model_X <- alpha_model_input %>%
  select(-alpha)

alpha_model_df$contract_no_seq_for_reg <- as.numeric(as.factor(alpha_model_df$contract_no))
alpha_model_df$project_bin_ids_for_reg <- as.numeric(alpha_model_df$potential_bidder_bin_id) ## these are already ids
alpha_model_df$project_type_id_seq_for_reg <- as.numeric(as.factor(alpha_model_df$project_type_id))
alpha_model_df$project_bid_open_year_rounded_seq_for_reg <- as.numeric(as.factor(alpha_model_df$bid_open_year_rounded))


contracts_df <- alpha_model_df %>%
  select(
    contract_no,
    alpha,
    contract_no_seq_for_reg,
    potential_bidder_bin_id,
    max_num_bidders,
    num_bids
  ) %>%
  mutate( ## this is truly the stupidest way to achieve this aim.
    first_bidder_in_auction_row = row_number()
  ) %>%
  group_by(contract_no) %>%
  mutate(
    min_alpha = min(alpha),
    max_alpha = max(alpha)
  ) %>%
  select(-alpha) %>%
  summarize_all(first) %>%
  ungroup() 


alpha_data_list <- list(
  N_I = nrow(alpha_model_X),
  N_n = nrow(contracts_df),
  N_PT = length(unique(alpha_model_df$project_type_id)),
  P = ncol(alpha_model_X),
  project_type_seqid = alpha_model_df$project_type_id_seq_for_reg,
  project_bin_seqid = contracts_df$potential_bidder_bin_id,
  project_seqid = alpha_model_df$contract_no_seq_for_reg,
  X_n = alpha_model_X,
  alpha_n_i = as.vector(alpha_model_df$alpha),
  min_alpha_in_auction = as.vector(contracts_df$min_alpha),
  max_alpha_in_auction = as.vector(contracts_df$max_alpha),
  first_bidder_in_auction_ids = as.vector(contracts_df$first_bidder_in_auction_row),
  num_potential_bidders = as.vector(contracts_df$max_num_bidders), #as.vector(contracts_df$max_num_bidders),
  num_entered_bidders = as.vector(contracts_df$num_bids)
)

### Try to Fit Alpha Distribution -- can skip too since optim hits locals fast
alpha_stan_model <- stan_model("estimation/4_entry_preprocess/4_alpha_lgnorm_reg_model_binomial_joint.stan")

alpha_model_fit <- sampling(alpha_stan_model, data = alpha_data_list, init = 0.,
                             cores = 8, iter=1000, control = list(max_treedepth = 15))

alpha_lgmean_fit <- get_posterior_mean(alpha_model_fit, pars = "alpha_lgmean")[, 5]
alpha_lgsigma_fit <- get_posterior_mean(alpha_model_fit, pars = "alpha_lgsigma")[, 5]
alpha_bar_scaler_fit <- get_posterior_mean(alpha_model_fit, pars = "alpha_bar_scaler")[, 5]
alpha_bar_fit <- get_posterior_mean(alpha_model_fit, pars = "alpha_bar")[, 5]
entry_prob_fit <- get_posterior_mean(alpha_model_fit, pars = "entry_prob")[, 5]

contracts_df$alpha_lgmean_fit <- alpha_lgmean_fit
contracts_df$alpha_lgsigma_fit <- alpha_lgsigma_fit
contracts_df$alpha_mean_fit <- exp(alpha_lgmean_fit)
contracts_df$alpha_bar_fit <- alpha_bar_fit

alpha_compare <- alpha_model_df %>%
  select(contract_no, alpha) %>%
  left_join(contracts_df)

alpha_distribution_df <- alpha_compare %>%
  select(-alpha, -contract_no_seq_for_reg, -first_bidder_in_auction_row) %>%
  unique() %>%
  rename(
    min_alpha_seen = min_alpha,
    max_alpha_seen = max_alpha,
    alpha_bar_bayes = alpha_bar_fit,
    lgalpha_mean = alpha_lgmean_fit,
    lgalpha_sd = alpha_lgsigma_fit
  ) %>%
  group_by(potential_bidder_bin_id) %>%
  mutate(
    alpha_bar_bar = 1.05*(max(alpha_bar_bayes)),
    alpha_min = 0.95*(min(min_alpha_seen))
  ) %>%
  ungroup() %>%
  left_join(potential_bidder_bin_features)

write_csv(alpha_distribution_df, paste0(cf_inputs_dir,"alpha_distribution.csv"))

alpha_model_seed <- get_seed(alpha_model_fit) ## 2098537088; 04/04/2022
alpha_model_inits <- get_inits(alpha_model_fit)

today = Sys.Date()
sink(paste0(cf_inputs_dir,"alpha_model_seedinfo.txt"), append=FALSE, split=FALSE)
print(paste0("Date: ", today))
print(paste0("Alpha model seed: ",alpha_model_seed))
print("\n")
print(paste0("Alpha model inits: "))
alpha_model_inits
sink()

saveRDS(alpha_model_fit, paste0(cf_inputs_dir,"alpha_model_fit.rds"))

alpha_model_fit <- readRDS(file = paste0(cf_inputs_dir,"alpha_model_fit.rds"))

### Get gamma fit objects too ###
bidder_type_df <- alpha_model_df %>%
  select(contract_no, bidder_id, alpha, gamma)

pois_gamma_reg = fepois(gamma ~ log(alpha) |contract_no, bidder_type_df)
pois_gamma_pred = predict(pois_gamma_reg, bidder_type_df)

pois_gamma_coef <- coef(pois_gamma_reg)[[1]]
poisgamma_fe_df <- as.data.frame(fixef(pois_gamma_reg)) %>%
  rename(
    proj_fe = contract_no
  ) %>%
  rownames_to_column("contract_no") %>%
  mutate(
    contract_no = as.integer(contract_no)
  )

bidder_type_df <- bidder_type_df %>%
  left_join(poisgamma_fe_df, by = "contract_no")

bidder_type_df <- bidder_type_df %>%
  mutate(
    gamma_predicted = exp(proj_fe + log(alpha)*pois_gamma_coef)
  )

gamma_dist_df <- bidder_type_df %>%
  mutate(
    pois_gamma_alphacoef = pois_gamma_coef,
    pois_gamma_projfe = proj_fe,
    gamma_predbyalpha = gamma_predicted
  ) %>%
  select(bidder_id,
         contract_no,
         pois_gamma_alphacoef,
         pois_gamma_projfe,
         gamma_predbyalpha
  )

gamma_coeffs_df <- gamma_dist_df %>%
  group_by(contract_no) %>%
  summarize(
    pois_gamma_alphacoef = first(pois_gamma_alphacoef),
    pois_gamma_projfe = first(pois_gamma_projfe)
  )

type_distribution_df <- read_csv(paste0(cf_inputs_dir,"alpha_distribution.csv")) %>%
  left_join(gamma_coeffs_df, by = "contract_no")

write_csv(type_distribution_df, paste0(cf_inputs_dir,"alpha_gamma_distribution.csv"))
