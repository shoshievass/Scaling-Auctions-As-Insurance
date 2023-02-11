##==================================================
##
## Script name: Get Calibrated Entry Params from Julia Grid Search Outputs
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script: Export an entry cost and EWO coefficient for each auction
##                    and output Table 5
##
##
## Input:  data/estimation_step5_outputs/step5_entry_calibration_output.csv
##         data/estimation_step4_cf_inputs/alpha_distribution.csv
##
##
## Output: data/estimation_step6_precf/calibrated_entry_params_binlevel.csv
##         outputs/cf_results/table5a.tex
##         outputs/cf_results/table5b.tex
##
##==================================================


## Import Libraries ##
library(tidyverse)
library(ggplot2)
library(kableExtra)

## Output Directory ##
output_dir = file.path("data","estimation_step6_precf")
dir.create(output_dir)

## Import results from Step 5 ##
df = read_csv(
  file.path("data","estimation_step5_outputs", "step5_entry_calibration_output.csv")
) %>%
  filter(alpha_bar_norm < 1e-6) %>%
  filter(!lower_than_lowest) 

bin_info = read_csv(
  file.path("data","estimation_step4_cf_inputs", "alpha_distribution.csv")
) 

## Bin Level ##
df_bins <- df %>%
  left_join(bin_info) %>%
  mutate(
    entry_norm_raw = (entry_pct - q),
    cdf_norm_raw = (alpha_bar_cdf - q)
  ) %>%
  select(contract_no,
         potential_bidder_bin_id,
         actual,
         alpha_bar_norm,
         entry_norm_raw,
         cdf_norm_raw,
         cdf_norm,
         k,
         lambda
         )


bin_entry_norms = df_bins %>% 
  select(-contract_no) %>%
  group_by(actual, potential_bidder_bin_id, k, lambda) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  mutate(
    all_norm = sqrt(entry_norm_raw^2)) %>% 
  ungroup() %>% 
  group_by(actual, potential_bidder_bin_id) %>%
  arrange(all_norm, lambda, -k) %>% 
  mutate(norm_rank = 1:n()) %>% 
  filter(norm_rank == 1) %>% 
  select(-norm_rank) %>%
  ungroup()

bin_entry_cdf_norms = df_bins %>% 
  select(-contract_no) %>%
  group_by(actual, potential_bidder_bin_id, k, lambda) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  mutate(
    all_norm = sqrt(entry_norm_raw^2 + cdf_norm_raw^2)) %>% 
  ungroup() %>% 
  group_by(actual, potential_bidder_bin_id) %>%
  arrange(all_norm, lambda, -k) %>% 
  mutate(norm_rank = 1:n()) %>% 
  filter(norm_rank == 1) %>% 
  select(-norm_rank) %>%
  ungroup()

### Export Table 5 : Kappa and Lambda estimates ###
### Note: The table in the paper colates tables 5a (kappa) and 5b (lambda) created below 

summary_col_names <- c('Project Type', 'Mean', 'SD', '25%', '50%', '75%')

### Table 5a ###
sink(file.path("outputs","cf_results","table5a.tex"), append=FALSE, split=FALSE)
bin_entry_cdf_norms %>%
  ungroup() %>%
  filter(!actual) %>%
  select(k) %>%
  mutate(
    project_type = "Entry Cost ($)",
    k = k*1000
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(.,probs=.25), Median = quantile(.,probs=.5), Q3 = quantile(.,probs=.75))
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names, .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  kable(format = "latex", booktabs = T, caption = "K Estimates") %>%
  kable_styling(latex_options =c("scale_down"))
sink()
### End of Table 5a ###

### Table 5b ###
sink(file.path("outputs","cf_results","table5b.tex"), append=FALSE, split=FALSE)

bin_entry_cdf_norms %>%
  ungroup() %>%
  filter(!actual) %>%
  select(lambda) %>%
  mutate(
    project_type = "lambda"
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(.,probs=.25), Median = quantile(.,probs=.5), Q3 = quantile(.,probs=.75))
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names, .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  kable(format = "latex", booktabs = T, caption = "Lambda Estimates") %>%
  kable_styling(latex_options =c("scale_down"))
sink()
### End of Table 5b ###

## Add the same calibrated parameters to `actual` quantity CF versions for consistent comparisons
estimated_bin_entry_cdf_norms_for_all <- bin_entry_cdf_norms %>% 
  filter(!actual) %>%
  bind_rows(bin_entry_cdf_norms %>% 
              filter(!actual) %>%
              mutate(actual = T)
  ) %>%
  left_join(bin_info %>% select(contract_no, potential_bidder_bin_id)) %>%
  select(
    contract_no,
    potential_bidder_bin_id,
    actual,
    lambda,
    k
  )

estimated_bin_entry_cdf_norms_for_all %>%
  write_csv(file.path(output_dir, "calibrated_entry_params_binlevel.csv"))

