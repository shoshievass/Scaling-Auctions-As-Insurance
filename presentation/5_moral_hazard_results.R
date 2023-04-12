## ==================================================
##
## Script name: Tables and Figures 5. Moral Hazard results
##
## Project: Scaling Auctions as Insurance
##
## Purpose of script:  Create data description tables and figures
##
## Input files:  data/estimation_step8_moralhazard/mh_results_nocap.csv
##               data/estimation_step4_cf_inputs/alpha_gamma_distribution.csv
##
## Output files: outputs/mh_results/app_table6.tex
##
## ==================================================


## Packages ##
library(tidyverse)
library(latex2exp)
library(ggthemes)
library(kableExtra)
library(fixest)

## directory name for inputs
input_dir <- file.path("data", "estimation_step8_moralhazard")

## directory names for outputs
output_dir <- file.path("outputs", "mh_results")
dir.create(output_dir)

saveLastFig <- function(fname) {
  fpath <- file.path(output_dir, paste0(fname, ".jpg"))
  ggsave(fpath)
}

## To skip annoying warnings if just running through to replicate
options(warn = -1)


w <- 1000

type_distribution_df <- read_csv(file.path("data", "estimation_step4_cf_inputs", "alpha_gamma_distribution.csv"))

truncate_data <- function(x, lb, ub) {
  lower_quant <- quantile(x, lb, na.rm = T)
  upper_quant <- quantile(x, ub, na.rm = T)
  x[x < lower_quant] <- NA
  x[x > upper_quant] <- NA
  x[is.infinite(x)] <- NA
  x[is.na(x)] <- NA
  return(x)
}

## Appendix Table 6 ##

## Table Column Names ##
summary_col_names_numbids <- c("Number of Bidders", "Mean", "SD", "25%", "50%", "75%")
cf_summary_col_names <- c("CF Type", "Outcome", "Mean", "SD", "25%", "50%", "75%")
summary_col_names <- c("Project Type", "Mean", "SD", "25%", "50%", "75%")


mh_results <- read_csv(file.path(input_dir, "mh_results_nocap.csv")) %>%
  mutate(
    actual = ifelse(str_detect(tag, "actual"), T, F),
    mh = ifelse(str_detect(tag, "mh"), T, F)
  ) %>%
  select(
    contract,
    expected_ce,
    expected_cost,
    tag,
    actual,
    mh
  ) %>%
  mutate_at(
    vars(ends_with("cost") | ends_with("ce")),
    function(x) {
      x * w
    }
  ) %>%
  rename(
    contract_no = contract
  )


baseline <- mh_results %>%
  mutate(
    baseline = ifelse(str_detect(tag, "baseline") | tag == "mh-actual-0.5" | tag == "mh-estimated-0.5" | tag == "mh-actual-1.0" | tag == "mh-estimated-1.0" | tag == "mh-estimated-Inf" | tag == "mh-actual-Inf", T, F)
  ) %>%
  filter(baseline) %>%
  transmute(
    contract_no = contract_no,
    baseline_expected_ce = expected_ce,
    baseline_expected_cost = expected_cost,
    actual = actual,
    mh = mh
  ) %>%
  group_by(contract_no, mh) %>%
  mutate(
    baseline_expected_ce_estimated = max(baseline_expected_ce * !actual),
    baseline_expected_cost_estimated = max(baseline_expected_cost * !actual),
  ) %>%
  ungroup()


cf_summary_col_names_by_outcome <- c("CF Type", "Moral Hazard?", "Outcome", "Mean", "SD", "25%", "50%", "75%")


sink(file.path(output_dir, "app_table6.tex"), append = FALSE, split = FALSE)

mh_results %>%
  filter(!actual) %>%
  left_join(baseline %>% filter(!actual)) %>%
  filter(!str_detect(tag, "minbid|norisk|0.001")) %>%
  transmute(
    tag = case_when(
      str_detect(tag, "baseline") ~ "Status Quo",
      tag == "mh-estimated" ~ "Status Quo",
      str_detect(tag, "norisk") ~ "No Risk",
      str_detect(tag, "lump") ~ "Lump Sum",
      str_detect(tag, "mixed-0.001") ~ "Lump Sum (M)",
      str_detect(tag, "mixed-1:2") ~ "2:1 Renegotiation",
      str_detect(tag, "mixed-fifty-fifty") ~ "50-50 Renegotiation",
      str_detect(tag, "minbid") ~ "Min Bid"
    ),
    `Moral Hazard` = ifelse(mh, "Yes", "No"),
    `% DOT Savings` = (baseline_expected_cost - expected_cost) / baseline_expected_cost * 100
  ) %>%
  filter(!str_detect(tag, "Status Quo")) %>%
  pivot_longer(
    -c(tag, `Moral Hazard`),
    names_to = c("outcome"),
    values_to = "value"
  ) %>%
  group_by(tag, `Moral Hazard`, outcome) %>%
  mutate(value = truncate_data(value, 0.01, 0.99)) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75)),
    na.rm = T
  ) %>%
  arrange(tag, `Moral Hazard`, outcome) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~cf_summary_col_names_by_outcome,
    .cols = everything()
  ) %>%
  ungroup() %>%
  arrange(desc(`CF Type`)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, format, big.mark = ",", scientific = FALSE) %>%
  kable(
    format = "latex",
    col.names = c("CF Type", "Moral Hazard?", "Outcome", "Mean", "SD", "25%", "50%", "75%"),
    booktabs = TRUE,
    align = "c",
    linesep = c("", "\\addlinespace"),
    caption = "A comparison of counterfactual savings with and without moral hazard"
  )

sink()

## End Table 6 ##
