## ==================================================
##
## Script name: Tables and Figures 4. Counterfactual results
##
## Project: Scaling Auctions as Insurance
##
## Purpose of script:  Create counterfactual result description tables and figs
##
## Input:  data/estimation_step7_counterfactuals/calibrated_entry_params_binlevel/aggregate_results.csv
##         data/estimation_step7_counterfactuals/calibrated_entry_params_binlevel/detailed_results.csv
##         data/estimation_step4_cf_inputs/alpha_gamma_distribution.csv
##
## Output: outputs/cf_results/
##         outputs/cf_results/app_table12.tex
##         outputs/cf_results/app_table13a.tex
##         outputs/cf_results/app_table13b.tex
##
## ==================================================

## Packages ##
library(tidyverse)
library(latex2exp)
library(ggthemes)
library(kableExtra)
library(fixest)

## directory name for inputs
input_dir <- file.path("data", "estimation_step7_counterfactuals")

## directory names for outputs
output_dir <- file.path("outputs", "cf_results")
dir.create(output_dir)

## To skip annoying warnings if just running through to replicate
options(warn = -1)

type_distribution_df <- read_csv(file.path("data", "estimation_step4_cf_inputs", "alpha_gamma_distribution.csv"))

w <- 1000

truncate_data <- function(x, lb, ub) {
  lower_quant <- quantile(x, lb, na.rm = T)
  upper_quant <- quantile(x, ub, na.rm = T)
  x[x < lower_quant] <- NA
  x[x > upper_quant] <- NA
  x[is.infinite(x)] <- NA
  x[is.na(x)] <- NA
  return(x)
}

## Table Column Names ##
summary_col_names_numbids <- c("Number of Bidders", "Mean", "SD", "25%", "50%", "75%")
cf_summary_col_names <- c("CF Type", "Outcome", "Mean", "SD", "25%", "50%", "75%")
cf_summary_col_names_by_outcome <- c("Outcome", "CF Type", "Mean", "SD", "25%", "50%", "75%")
summary_col_names <- c("Project Type", "Mean", "SD", "25%", "50%", "75%")


### Appendix Table 12: Threshold Types  ###
cf_alphas <- read_csv(
  file.path(input_dir, "aggregate_results.csv") ## previously this was called `monopoly_results.csv`
) %>%
  filter(
    !str_detect(tag, "bkp"),
    !str_detect(tag, "mixed-lump"),
    !str_detect(tag, "mixed-0.001"),
  ) %>%
  select(
    contract_no, tag, is_actual, alpha_bar_prime
  ) %>%
  rename(actual = is_actual) %>%
  left_join(
    type_distribution_df %>%
      select(
        contract_no,
        pois_gamma_alphacoef,
        pois_gamma_projfe
      )
  ) %>%
  mutate(
    gamma_bar_prime = exp(pois_gamma_alphacoef * log(alpha_bar_prime) + pois_gamma_projfe)
  )

estimated_alpha_primes <- cf_alphas %>%
  filter(!actual) %>%
  filter(tag == "baseline") %>%
  transmute(
    contract_no = contract_no,
    baseline_alpha_bar_prime = alpha_bar_prime,
    baseline_gamma_bar_prime = gamma_bar_prime
  )

sink(file.path(output_dir, "app_table12.tex"), append = FALSE, split = FALSE)
cf_alphas %>%
  filter(!actual) %>%
  filter(!str_detect(tag, "minbid|baseline|norisk-0.001")) %>%
  select(
    contract_no,
    tag,
    alpha_bar_prime,
    gamma_bar_prime
  ) %>%
  left_join(estimated_alpha_primes, by = c("contract_no")) %>%
  mutate(
    d_alphabar = 100 * (alpha_bar_prime - baseline_alpha_bar_prime) / baseline_alpha_bar_prime,
    d_gammabar = 100 * (gamma_bar_prime - baseline_gamma_bar_prime) / baseline_gamma_bar_prime
  ) %>%
  select(tag, d_alphabar, d_gammabar) %>%
  pivot_longer(
    -tag,
    names_to = "param",
    values_to = "delta"
  ) %>%
  group_by(tag, param) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  mutate(
    tag = case_when(
      str_detect(tag, "baseline") ~ "Status Quo",
      str_detect(tag, "norisk") ~ "No Risk",
      str_detect(tag, "norisk-0.001") ~ "No Risk (M)",
      str_detect(tag, "lump") ~ "Lump Sum",
      str_detect(tag, "mixed-0.001") ~ "Lump Sum",
      str_detect(tag, "mixed-1:2") ~ "1:2 Renegotiation",
      str_detect(tag, "mixed-2:1") ~ "2:1 Renegotiation",
      str_detect(tag, "mixed-fifty-fifty") ~ "50-50 Renegotiation",
      str_detect(tag, "minbid") ~ "Min Bid"
    ),
    param = case_when(
      str_detect(param, "alpha") ~ "Cost Efficiency (alpha)",
      str_detect(param, "gamma") ~ "Risk Aversion (gamma)"
    )
  ) %>%
  arrange(param, tag) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  arrange(
    Outcome,
    factor(`CF Type`, levels = c(
      "Lump Sum",
      "Lump Sum (M)",
      "Lump Sum w 2:1 Negotiation",
      "Lump Sum w 50/50 Negotiation",
      "No Risk (Correct q)",
      "No Risk (Estimated q)"
    ), ordered = T)
    # Outcome
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, format, big.mark = ",", scientific = FALSE) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "l",
    linesep = c("", "", "", "", "\\addlinespace \\addlinespace"),
    caption = "Summary Statistics for CF Threshold Type Changes under Endogenous Entry"
  )
sink()
### End of Table 12 ###


### Appendix Table 13: Comparison of Counterfactual Equilibrium Outcomes ###
### Note: Table 13 in the paper rearranges some sections and colates 13a (with endogenous entry) and 13b (no endogenous entry)

cf_results_by_n <- read_csv(file.path(input_dir, "detailed_results.csv")) ## previously labeled `raw.csv`

cf_agg <- cf_results_by_n %>%
  filter(
    !str_detect(tag, "bkp"),
    !str_detect(tag, "mixed-lump"),
    !str_detect(tag, "mixed-0.001"),
  ) %>%
  mutate(
    expected_cost = ifelse(expected_cost < 0, 0, expected_cost) * w,
    expected_ce = ifelse(expected_ce < 0, 0, expected_ce) * w,
  ) %>%
  group_by(contract_no, tag, is_prime, actual) %>%
  summarize(
    k = first(k),
    lambda = first(lambda),
    N = first(N),
    observed_bidders = first(observed_bidders),
    expected_cost = sum(expected_cost * weighting),
    expected_ce = sum(expected_ce * weighting)
  ) %>%
  ungroup() %>%
  left_join(cf_alphas)

# Extract baseline costs
baseline <- cf_agg %>%
  filter(str_detect(tag, "baseline"), !str_detect(tag, "bkp")) %>%
  transmute(
    contract_no = contract_no,
    baseline_expected_ce = expected_ce,
    baseline_expected_cost = expected_cost,
    baseline_alpha_bar_prime = alpha_bar_prime,
    actual,
    is_prime
  ) %>%
  group_by(contract_no, is_prime) %>%
  mutate(
    baseline_expected_ce_estimated = max(baseline_expected_ce * !actual),
    baseline_expected_cost_estimated = max(baseline_expected_cost * !actual),
    baseline_alpha_bar_prime_estimated = max(baseline_alpha_bar_prime * !actual),
  ) %>%
  ungroup()

baseline_aucs <- baseline %>%
  select(contract_no) %>%
  unique() %>%
  pull(contract_no)



## Baseline Estimated + Endogenous Entry Results ##
lump_prime_results <- cf_agg %>%
  filter(!actual & is_prime) %>%
  filter(contract_no %in% baseline_aucs) %>%
  filter(str_detect(tag, "mixed") | str_detect(tag, "lump")) %>%
  filter(!str_detect(tag, "mixed-2:1")) %>%
  left_join(baseline %>% filter(!actual & is_prime)) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "baseline") ~ "Status Quo",
      str_detect(tag, "norisk") ~ "No Risk (Estimated q)",
      str_detect(tag, "lump") ~ "Lump Sum",
      str_detect(tag, "mixed-1:2") ~ "Lump Sum w 2:1 Negotiation",
      str_detect(tag, "mixed-2:1") ~ "2:1 Renegotiation",
      str_detect(tag, "mixed-fifty-fifty") ~ "Lump Sum w 50/50 Negotiation",
    ),
    `% Efficiency Threshold` = (alpha_bar_prime - baseline_alpha_bar_prime) / baseline_alpha_bar_prime * 100,
    `$ DOT Savings` = baseline_expected_cost - expected_cost,
    `% DOT Savings` = (baseline_expected_cost - expected_cost) / baseline_expected_cost * 100,
    `$ Bidder Gain` = expected_ce - baseline_expected_ce,
    `% Bidder Gain` = (expected_ce - baseline_expected_ce) / baseline_expected_ce * 100
  ) %>%
  pivot_longer(
    -tag,
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  ungroup()

norisk_actual_prime_results <- cf_agg %>%
  filter(actual & is_prime) %>%
  filter(contract_no %in% baseline_aucs) %>%
  filter(str_detect(tag, "norisk")) %>%
  left_join(baseline) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "norisk") ~ "No Risk (Correct q)"
    ),
    `% Efficiency Threshold` = (alpha_bar_prime - baseline_alpha_bar_prime) / baseline_alpha_bar_prime * 100,
    `$ DOT Savings` = baseline_expected_cost - expected_cost,
    `% DOT Savings` = (baseline_expected_cost - expected_cost) / baseline_expected_cost * 100,
    `$ Bidder Gain` = expected_ce - baseline_expected_ce,
    `% Bidder Gain` = (expected_ce - baseline_expected_ce) / baseline_expected_ce * 100
  ) %>%
  pivot_longer(
    -tag,
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  ungroup()


estimated_norisk_prime_results <- cf_agg %>%
  filter(actual & is_prime) %>%
  filter(contract_no %in% baseline_aucs) %>%
  filter(str_detect(tag, "norisk")) %>%
  left_join(baseline) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "norisk") ~ "No Risk (Estimated q)"
    ),
    `% Efficiency Threshold` = (alpha_bar_prime - baseline_alpha_bar_prime_estimated) / baseline_alpha_bar_prime_estimated * 100,
    `$ DOT Savings` = -expected_cost + baseline_expected_cost_estimated,
    `% DOT Savings` = `$ DOT Savings` / baseline_expected_cost_estimated * 100,
    `$ Bidder Gain` = -expected_ce + baseline_expected_ce_estimated,
    `% Bidder Gain` = `$ Bidder Gain` / baseline_expected_ce_estimated * 100
  ) %>%
  pivot_longer(
    -tag,
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  ungroup()

## Table 13 a: Accounting for Endogenous Entry ##
## Note: The outcomes are manually shuffled to a more intuitive order in the paper ##
sink(file.path(output_dir, "app_table13a.tex"), append = FALSE, split = FALSE)

lump_prime_results %>%
  bind_rows(norisk_actual_prime_results) %>%
  bind_rows(estimated_norisk_prime_results) %>%
  arrange(
    Outcome,
    factor(Outcome, levels = c(
      "% Efficiency Threshold",
      "% DOT Savings",
      "$ DOT Savings",
      "% Bidder Gain",
      "$ Bidder Gain"
    ), ordered = T),
    factor(`CF Type`, levels = c(
      "Lump Sum",
      "Lump Sum (M)",
      "Lump Sum w 2:1 Negotiation",
      "Lump Sum w 50/50 Negotiation",
      "No Risk (Correct q)",
      "No Risk (Estimated q)"
    ), ordered = T)
  ) %>%
  filter(Outcome != "% Efficiency Threshold") %>%
  mutate_if(is.numeric, round, 1) %>%
  mutate_if(is.numeric, format, big.mark = ",", scientific = FALSE) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "l",
    linesep = c("", "", "", "", "\\addlinespace \\addlinespace"),
    caption = "Summary Statistics for CFs with Entry"
  )
sink()

#########################

## Baseline Estimated + No Prime Results ##
lump_noprime_results <- cf_agg %>%
  filter(!actual & !is_prime) %>%
  filter(contract_no %in% baseline_aucs) %>%
  filter(str_detect(tag, "mixed") | str_detect(tag, "lump")) %>%
  filter(!str_detect(tag, "mixed-2:1")) %>%
  left_join(baseline %>% filter(!actual & !is_prime)) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "baseline") ~ "Status Quo",
      str_detect(tag, "norisk") ~ "No Risk (Estimated q)",
      str_detect(tag, "lump") ~ "Lump Sum",
      str_detect(tag, "mixed-0.001") ~ "Lump Sum (M)",
      str_detect(tag, "mixed-1:2") ~ "Lump Sum w 2:1 Negotiation",
      str_detect(tag, "mixed-2:1") ~ "2:1 Renegotiation",
      str_detect(tag, "mixed-fifty-fifty") ~ "Lump Sum w 50/50 Negotiation",
      str_detect(tag, "minbid") ~ "Min Bid"
    ),
    `% Efficiency Threshold` = (alpha_bar_prime - baseline_alpha_bar_prime) / baseline_alpha_bar_prime * 100,
    `$ DOT Savings` = baseline_expected_cost - expected_cost,
    `% DOT Savings` = (baseline_expected_cost - expected_cost) / baseline_expected_cost * 100,
    `$ Bidder Gain` = expected_ce - baseline_expected_ce,
    `% Bidder Gain` = (expected_ce - baseline_expected_ce) / baseline_expected_ce * 100
  ) %>%
  pivot_longer(
    -tag,
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  ungroup()

norisk_actual_noprime_results <- cf_agg %>%
  filter(actual & !is_prime) %>%
  filter(contract_no %in% baseline_aucs) %>%
  filter(str_detect(tag, "norisk")) %>%
  left_join(baseline) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "norisk") ~ "No Risk (Correct q)"
    ),
    `% Efficiency Threshold` = (alpha_bar_prime - baseline_alpha_bar_prime) / baseline_alpha_bar_prime * 100,
    `$ DOT Savings` = baseline_expected_cost - expected_cost,
    `% DOT Savings` = (baseline_expected_cost - expected_cost) / baseline_expected_cost * 100,
    `$ Bidder Gain` = expected_ce - baseline_expected_ce,
    `% Bidder Gain` = (expected_ce - baseline_expected_ce) / baseline_expected_ce * 100
  ) %>%
  pivot_longer(
    -tag,
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  ungroup()


estimated_norisk_noprime_results <- cf_agg %>%
  filter(actual & !is_prime) %>%
  filter(contract_no %in% baseline_aucs) %>%
  filter(str_detect(tag, "norisk")) %>%
  left_join(baseline) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "norisk") ~ "No Risk (Estimated q)"
    ),
    `% Efficiency Threshold` = (alpha_bar_prime - baseline_alpha_bar_prime_estimated) / baseline_alpha_bar_prime_estimated * 100,
    `$ DOT Savings` = -expected_cost + baseline_expected_cost_estimated,
    `% DOT Savings` = `$ DOT Savings` / baseline_expected_cost_estimated * 100,
    `$ Bidder Gain` = -expected_ce + baseline_expected_ce_estimated,
    `% Bidder Gain` = `$ Bidder Gain` / baseline_expected_ce_estimated * 100
  ) %>%
  pivot_longer(
    -tag,
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names,
    .cols = everything()
  ) %>%
  ungroup()

## Table 13 b: Not Accounting for Endogenous Entry ##
sink(file.path(output_dir, "app_table13b.tex"), append = FALSE, split = FALSE)

lump_noprime_results %>%
  bind_rows(norisk_actual_noprime_results) %>%
  bind_rows(estimated_norisk_noprime_results) %>%
  arrange(
    Outcome,
    factor(Outcome, levels = c(
      "% Efficiency Threshold",
      "% DOT Savings",
      "$ DOT Savings",
      "% Bidder Gain",
      "$ Bidder Gain"
    ), ordered = T),
    factor(`CF Type`, levels = c(
      "Lump Sum",
      "Lump Sum (M)",
      "Lump Sum w 2:1 Negotiation",
      "Lump Sum w 50/50 Negotiation",
      "No Risk (Correct q)",
      "No Risk (Estimated q)"
    ), ordered = T)
  ) %>%
  filter(Outcome != "% Efficiency Threshold") %>%
  mutate_if(is.numeric, round, 1) %>%
  mutate_if(is.numeric, format, big.mark = ",", scientific = FALSE) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "l",
    linesep = c("", "", "", "", "\\addlinespace \\addlinespace"),
    caption = "Summary Statistics for CFs without Entry"
  )
sink()

### End of Table 13 ###
