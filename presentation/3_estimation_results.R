## ==================================================
##
## Script name: Tables and Figures 3. Estimation Results
##
## Project: Scaling Auctions as Insurance
##
## Purpose of script:  Create data description tables and figures
##
## Input:   data/estimation_step1_output_minfit.rdata
##          data/estimation_step3/second_stage_estimates_summary.csv
##
## Output:  outputs/estimation_results/table3a.tex
##          outputs/estimation_results/table3b.tex
##          outputs/estimation_results/table4a.tex
##          outputs/estimation_results/table4b.tex
##          outputs/estimation_results/app_table11.tex
##          outputs/estimation_results/fig5a.jpg
##          outputs/estimation_results/fig5b.jpg
##          outputs/estimation_results/app_fig9.jpg
##
## ==================================================


## Packages ##
library(tidyverse)
library(latex2exp)
library(ggthemes)
library(kableExtra)
library(fixest)
library(mltools)

# Create .library if it doesn't exist
if (!dir.exists("presentation/.library")) {
  dir.create("presentation/.library")
}

# Check if binscattr is installed
if (!requireNamespace("binscattr", lib.loc = "presentation/.library/")) {
  # Tell the user we're installing binscattr
  message("Installing binscattr to presentation/.library/")

  # If not, install it
  devtools::install_local(
    "presentation/binscattr/binscattr/",
    upgrade = "ask",
    force = TRUE,
    lib = "presentation/.library/",
    build_vignettes = FALSE
  )
}

# Load binscattr
library(binscattr, lib.loc = "presentation/.library/")

## Load the data
### 1st stage estimates + transformed input data
load(file.path("data", "estimation_step1_output_minfit.rdata"))
### 2nd stage estimates
gmm_output <- read_csv(file.path("data", "estimation_step3", "second_stage_estimates_summary.csv"))

## directory names for outputs
output_dir <- file.path("outputs", "estimation_results")
dir.create(output_dir)

ASPECT_RATIO <- 3/4

saveLastFig <- function(fname){
  fpath = file.path(output_dir, paste0(fname, ".jpg"))
  ggsave(fpath, height=7, width=7 / ASPECT_RATIO)
}

## To skip annoying warnings if just running through to replicate
options(warn = -1)


## Table Column Names ##
summary_col_names <- c("Project Type", "Mean", "SD", "25%", "50%", "75%")
summary_col_names_numbids <- c("Number of Bidders", "Mean", "SD", "25%", "50%", "75%")

##
gmm_output <- gmm_output %>%
  mutate(
    num_bids = ifelse(num_bids > 9, 10, num_bids),
    dollar_scale = 1000
  ) %>%
  rename(
    bidder_markup = mkp
  ) %>%
  mutate(
    num_bids = factor(num_bids)
  )

id_map <- empirical_bid_data %>%
  select(project_bidder_id, contract_no, project_type, num_bidders, rank) %>%
  group_by(project_bidder_id) %>%
  summarize_all(first)

cost_df <- bidder_project_level_df %>%
  left_join(id_map) %>%
  select(
    contract_no,
    bidder_id_seq,
    project_bidder_id,
    project_type,
    extra_work_payment,
    bidder_score,
    num_bidders,
    rank
  ) %>%
  rename(
    raw_num_bids = num_bidders,
    raw_rank = rank
  ) %>%
  mutate(
    grouped_rank = ifelse(raw_rank < 10, raw_rank, 10),
    ewo = extra_work_payment / 1000
  ) %>%
  left_join(gmm_output, by = c("contract_no" = "contract_no", "bidder_id_seq" = "bidder_id")) %>%
  filter(complete.cases(.))

bidder_ids <- bidder_project_level_df %>% select(project_bidder_id, bidder_id_seq)

rank_df <- empirical_bid_data %>%
  select(project_bidder_id, contract_no, bridge, rank) %>%
  group_by(project_bidder_id) %>%
  summarize_all(first) %>%
  left_join(bidder_ids, by = "project_bidder_id") %>%
  transmute(
    contract_no = contract_no,
    bidder_id = bidder_id_seq,
    rank = rank
  )

## Fig 5a ##
demo_project_item_df %>% ggplot(aes(x = sigma_t_fit)) +
  geom_histogram() +
  labs(
    x = TeX("Quantity Standard Deviation: $\\sigma_{t,n}$"),
    y = "Count"
  ) +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  ) + theme(aspect.ratio = ASPECT_RATIO)
saveLastFig("fig5a")
## End of Fig 5a ##

## Fig 5b: Alpha vs Log(Gamma) Plot ##
df <- gmm_output %>%
  left_join(rank_df, by = c("contract_no", "bidder_id")) %>%
  mutate(
    winner = ifelse(rank == 1, "Winner", "Loser"),
    inv_gamma = 1.0 / gamma,
    log_gamma = log(gamma),
    log_alpha = log(alpha),
    log_inv_gamma = log(inv_gamma)
  )

df <- df %>%
  group_by(contract_no) %>%
  mutate(
    mean_alpha = mean(alpha),
    mean_lgalpha = mean(log(alpha)),
    mean_gamma = mean(gamma),
    mean_inv_gamma = mean(inv_gamma),
    mean_lggamma = mean(log(gamma)),
  ) %>%
  ungroup() %>%
  mutate(
    demeaned_alpha = alpha - mean_alpha,
    demeaned_lgalpha = log(alpha) - mean_lgalpha,
    demeaned_gamma = gamma - mean_gamma,
    demeaned_invgamma = inv_gamma - mean_inv_gamma,
    demeaned_lggamma = log(gamma) - mean_lggamma,
  ) %>%
  mutate(
    overall_mean_alpha = mean(alpha),
    overall_mean_gamma = mean((gamma)),
    overall_mean_lggamma = mean(log(gamma)),
    overall_mean_invgamma = mean(inv_gamma),
    remeaned_alpha = demeaned_alpha + overall_mean_alpha,
    remeaned_lggamma = demeaned_lggamma + overall_mean_lggamma,
    remeaned_gamma = exp(remeaned_lggamma),
    remeaned_raw_gamma = demeaned_gamma + overall_mean_gamma,
  )

df %>%
  binscatter_manual(
    x = remeaned_alpha, y = remeaned_gamma,
    bins = 25,
    pos = "none"
  ) +
  labs(
    x = TeX("Efficiency $\\alpha$"), # "Efficiency Type  (demeaned by contract)",
    y = TeX("Risk Aversion $\\gamma$") # "Risk Aversion (log-scale, demeaned by contract)"
  ) +
  theme_minimal() +
  theme(text = element_text(size=24),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) + 
  theme(aspect.ratio = ASPECT_RATIO)

saveLastFig("fig5b")
## End of Fig 5b ##


## Table 3: Alpha and Gamma estimate tables##
## Note: tables 3a and 3b were colated manually for the paper ##

## Table 3a: Gamma Estimates ##
sink(file.path(output_dir, "table3a.tex"), append = FALSE, split = FALSE)

cost_df %>%
  select(
    gamma
  ) %>%
  mutate(
    project_type = "All"
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names,
    .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 3) %>%
  kable(format = "latex", booktabs = T, caption = "Aggregate Summary statistics of gamma estimates") %>%
  kable_styling(latex_options = c("scale_down"))

cost_df %>%
  select(
    project_type,
    gamma
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  mutate(
    project_type = as.character(project_type)
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names,
    .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 3) %>%
  kable(format = "latex", booktabs = T, caption = "Aggregate Summary statistics of gamma estimates") %>%
  kable_styling(latex_options = c("scale_down"))

sink()

## Table 3b: Alpha estimate table ##
sink(file.path(output_dir, "table3b.tex"), append = FALSE, split = FALSE)

cost_df %>%
  select(
    alpha
  ) %>%
  mutate(
    project_type = "All"
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names,
    .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 3) %>%
  kable(format = "latex", booktabs = T, caption = "Aggregate Summary statistics of alpha estimates") %>%
  kable_styling(latex_options = c("scale_down"))

cost_df %>%
  select(
    project_type,
    alpha
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  mutate(
    project_type = as.character(project_type)
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names,
    .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 3) %>%
  kable(format = "latex", booktabs = T, caption = "Aggregate Summary statistics of alpha estimates") %>%
  kable_styling(latex_options = c("scale_down"))

sink()
## End of Table 3 ##


## Table 4: Markups ##
## Note: Table 4 in the paper manually colates tables 4a and 4b produced below ##

## Table 4a ##
sink(file.path(output_dir, "table4a.tex"), append = FALSE, split = FALSE)
cost_df %>%
  select(
    bidder_markup,
    project_type
  ) %>%
  mutate(
    bidder_markup = bidder_markup * 100,
    project_type = "All"
  ) %>%
  group_by(project_type) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  rename(
    `Project Type` = project_type
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names,
    .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 0) %>%
  mutate_if(is.numeric, paste0, "%") %>%
  kable(format = "latex", booktabs = T, caption = "Summary statistics of estimated bidder markups") %>%
  kable_styling(latex_options = c("scale_down"))
sink()

sink(file.path(output_dir, "table4b.tex"), append = FALSE, split = FALSE)
cost_df %>%
  mutate(
    grouped_num_bids = case_when(
      raw_num_bids < 4 ~ "2-3 Bidders",
      raw_num_bids < 7 ~ "4-6 Bidders",
      T ~ "7+ Bidders"
    )
  ) %>%
  select(
    grouped_num_bids,
    bidder_markup
  ) %>%
  mutate(
    bidder_markup = bidder_markup * 100
  ) %>%
  group_by(grouped_num_bids) %>%
  summarize_all(
    funs(Mean = mean, SD = sd, Q1 = quantile(., probs = .25), Median = quantile(., probs = .5), Q3 = quantile(., probs = .75))
  ) %>%
  rename(
    `Number of Bidders` = grouped_num_bids
  ) %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~summary_col_names_numbids,
    .cols = everything()
  ) %>%
  mutate_if(is.numeric, round, 0) %>%
  mutate_if(is.numeric, paste0, "%") %>%
  kable(format = "latex", booktabs = T, caption = "Summary statistics of estimated winning bidder markups") %>%
  kable_styling(latex_options = c("scale_down"))
sink()


## Appendix Table 11 ##
pois_gamma_reg <- fepois(gamma ~ log(alpha) | contract_no, df)
df$predicted_poisgamma <- predict(pois_gamma_reg, df)

sink(file.path(output_dir, "app_table11.tex"), append = FALSE, split = FALSE)

gamma_fit_lm <- (lm(gamma ~ predicted_poisgamma, df))
stargazer::stargazer(gamma_fit_lm, dep.var.labels = "Gamma", covariate.labels = "Predicted Gamma", omit.stat = c("adj.rsq", "f", "ser"))

sink()
## End Table 11 ##

## Appendix Figure 9 ##
library(latex2exp)

reg_sum <- df %>%
  transmute(
    resids = (predicted_poisgamma) - (gamma),
    lg_resids = log(predicted_poisgamma) - log(gamma),
    pct_lgresids = (lg_resids) / log(gamma),
    sds_lgresids = lg_resids / sd(log(gamma)),
    sds_resids = resids / sd(gamma)
  )

quantile(reg_sum$sds_resids, seq(0, 1, .25))
sd_resid_quantiles <- quantile(reg_sum$sds_resids, seq(0, 1, .05))
reg_sum %>%
  filter(sds_resids > sd_resid_quantiles[2] & sds_resids < sd_resid_quantiles[20]) %>%
  ggplot(aes(x = sds_resids)) +
  geom_histogram() +
  scale_x_continuous() +
  labs(
    x = TeX("Residuals in Units of ($\\gamma)$ Standard Deviations"),
    y = "Count"
  ) +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)), 
        aspect.ratio = ASPECT_RATIO
  )

saveLastFig("app_fig9")

## End Figure 9 ##
