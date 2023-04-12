## ==================================================
##
## Script name: Tables and Figures 1. Data description
##
## Project: Scaling Auctions as Insurance
##
## Purpose of script:  Create data description tables and figures
##
## Input: data/estimation_step1_output_minfit.rdata
##
## Output: outputs/data_description/
##         outputs/data_description/table1.tex
##         outputs/data_description/table2.tex
##         outputs/data_description/figure1.jpg
##         outputs/data_description/figure2a.jpg
##         outputs/data_description/figure2b.jpg
##
## ==================================================

## Libraries ##
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(glue)
library(stargazer)

## To skip annoying warnings if just running through to replicate
options(warn = -1)

## Load the data
### 1st stage estimates + transformed input data
load(file.path("data", "estimation_step1_output_minfit.rdata"))

## directory names for outputs
output_dir <- file.path("outputs", "data_description")
dir.create(output_dir, recursive = TRUE)

saveLastFig <- function(fname) {
  fpath <- file.path(output_dir, paste0(fname, ".jpg"))
  ggsave(fpath)
}

## Table 1 : Summary Statistics ##
## Note: For the sake of clean presentation, some manual changes to column labels, row ordering and rounding were done to generate the exact table in the paper.
summary_df <- empirical_bid_data %>%
  select(
    contract_no,
    bidder_id_sequential,
    q_at,
    q_ot,
    office_unit_price,
    bid_unit_price,
    bid_open_year,
    rank,
    proj_duration,
    num_bidders,
    extra_work_payment
  ) %>%
  group_by(
    contract_no, bidder_id_sequential
  ) %>%
  mutate(
    office_score = sum(office_unit_price * q_ot),
    bidder_score = sum(bid_unit_price * q_ot),
    office_expost = sum(office_unit_price * q_at),
    bidder_revenue = sum(bid_unit_price * q_at),
    num_items = n()
  ) %>%
  dplyr::filter(
    rank == 1
  ) %>%
  summarize_all(first) %>%
  select(
    contract_no,
    proj_duration,
    bid_open_year,
    office_score,
    bidder_score,
    office_expost,
    bidder_revenue,
    num_items,
    num_bidders,
    extra_work_payment
  )

sink(file.path(output_dir, "table1.tex"), append = FALSE, split = FALSE)
summary_df %>%
  ungroup() %>%
  mutate(
    `Net Over-Cost (DOT Quantities) ($)` = bidder_revenue - office_score,
    `Net Over-Cost (Ex-Post Quantities) ($)` = bidder_revenue - office_expost,
    `Net Over-Cost (Ex-Post Quantities) (%)` = 100 * (bidder_revenue - office_expost) / office_expost,
    proj_duration = proj_duration / 364
  ) %>%
  select(
    -contract_no,
    -bid_open_year,
    -bidder_score,
    -bidder_revenue,
    -office_expost,
  ) %>%
  rename(
    `Project Value (DOT Estimate)` = office_score,
    `Types of Materials` = num_items,
    `Project Length (Estimate)` = proj_duration,
    `Number of Bidders` = num_bidders,
    `Extra Work Orders` = extra_work_payment
  ) %>%
  as.data.frame() %>%
  mutate(
    `Num Auctions` = n()
  ) %>%
  stargazer::stargazer(
    digits = 2,
    summary.stat = c("mean", "sd", "p25", "median", "p75")
  )

sink()
### End of Table 1 ###


### Figure 1: Net Over-Cost (ex-post quantities) across bridge projects ###
summary_df %>%
  ungroup() %>%
  mutate(
    `$ Overruns` = (bidder_revenue - office_score)
  ) %>%
  ggplot(aes(x = `$ Overruns`)) +
  scale_x_continuous(labels = scales::dollar, lim = c(-4e+06, 4e+06)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "Overruns (Dollars)",
    y = "Count"
  ) +
  theme(axis.title = element_text(size = 20)) +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(hjust = 0.7)
  )
saveLastFig("fig1")

### End of Fig 1 ##

### Table 2 : Bidder Description ###
## Note: For the sake of clean presentation, some manual changes to column labels, row ordering and rounding were done to generate the exact table in the paper.
firm_df <- bidder_project_level_df %>%
  group_by(bidder_id_seq) %>%
  mutate(num_aucs = n()) %>%
  ungroup() %>%
  mutate(
    fringe_num_aucs = ifelse(num_aucs < 30, 1, 0),
    bidder_id_grouped_raw = ifelse(num_aucs < 30, 999, bidder_id_seq)
  ) %>%
  mutate(
    bidder_id_grouped_seq = as.numeric(factor(bidder_id_grouped_raw))
  ) %>%
  select(
    project_bidder_id,
    fringe_num_aucs
  )

more_project_chars <- bidder_project_level_df %>%
  select(
    project_bidder_id,
    designed_inhouse,
    office_score_estimate,
    bid_dot_expost_quantity,
    bidder_score,
    office_cost_dot_expost_quantity
  ) %>%
  unique()

bidder_project_chars <- empirical_bid_data %>%
  left_join(
    firm_df,
    by = c("project_bidder_id")
  ) %>%
  select(
    bidder_id_sequential,
    project_bidder_id,
    contract_no,
    bidder_specialization,
    fringe,
    utilization,
    capacity,
    same_district,
    project_type_id,
    fringe_num_aucs,
    rank
  ) %>%
  group_by(project_bidder_id) %>%
  summarize_all(first) %>%
  left_join(more_project_chars, by = "project_bidder_id") %>%
  ungroup()

bidder_project_desc <- bidder_project_chars

bidder_chars <- bidder_project_desc %>%
  mutate(
    winner = ifelse(rank == 1, 1, 0)
  ) %>%
  group_by(bidder_id_sequential) %>%
  mutate(
    num_auctions = n(),
    num_wins = sum(winner)
  ) %>%
  ungroup() %>%
  select(-project_bidder_id, -contract_no) %>%
  group_by(bidder_id_sequential) %>%
  summarize_all(
    .funs = "mean"
  )

bidder_project_chars %>%
  group_by(project_type_id) %>%
  summarize(
    num_bids = n(),
    num_fringe = sum(fringe),
    prop_fringe = sum(fringe) / num_bids,
    num_dom = sum(1 - fringe)
  )

sum_by_fringe <-
  bidder_chars %>%
  select(
    fringe_num_aucs,
    num_auctions,
    num_wins,
    winner,
    fringe,
    bidder_specialization,
    same_district,
    utilization,
    bidder_score,
    office_cost_dot_expost_quantity,
    bid_dot_expost_quantity,
    capacity
  ) %>%
  mutate(
    `Common Firm` = ifelse(fringe_num_aucs == 0, "Common Firm", "Rare Firm")
  ) %>%
  group_by(
    `Common Firm`
  ) %>%
  mutate(
    num_firms = n()
  ) %>%
  summarize(
    `Number of Firms` = first(num_firms),
    `Total Number of Bid Submitted` = sum(num_auctions),
    `Mean Number of Bid Submitted Per Firm` = mean(num_auctions),
    `Median Number of Bid Submitted Per Firm` = median(num_auctions),
    `Total Number of Wins` = sum(num_wins),
    `Mean Number of Wins Per Firm` = mean(num_wins),
    `Median Number of Wins Per Firm` = median(num_wins),
    `Mean Bid Submitted` = mean(bidder_score),
    `Mean Ex-Post Cost of Bid` = mean(bid_dot_expost_quantity),
    `Mean Ex-Post Overrun of Bid` = mean(100 * (bid_dot_expost_quantity - office_cost_dot_expost_quantity) / office_cost_dot_expost_quantity),
    `Mean Capacity` = mean(capacity),
    `Percent of Bids on Projects in the Same District` = mean(same_district) * 100,
    `Percent of Bids by Revenue Dominant Firms` = mean((1 - fringe)) * 100,
    `Mean Utilization Ratio` = mean(utilization) * 100,
    `Mean Specialization` = mean(bidder_specialization) * 100
  ) %>%
  mutate_if(is.numeric, round, 2)

sink(file.path(output_dir, "table2.tex"), append = FALSE, split = FALSE)
t(sum_by_fringe) %>%
  stargazer::stargazer(
    title = "Comparison of Firms Participating in <30 vs 30+ Auctions",
    summary = F,
    digits = 2
  )
sink()

### End of Table 2 ###


### Figure 2 a ####
item_chars_df <- demo_project_item_df %>%
  select(
    contract_no,
    project_item_id,
    item_id_sequential,
    old_item_id,
    q_at,
    q_ot,
    project_type_id,
    proj_manager_id,
    engineer_id,
    designer_id,
    district,
    top_skew_item,
    tot_top_skew_item,
    unit_type_be,
    office_unit_price
  ) %>%
  mutate(
    q_at_model = as.vector(qa_model_fit),
    sigma_t = as.vector(sigma_t_fit)
  ) %>%
  ungroup() %>%
  mutate(
    diff_q = (q_at - q_ot) / q_ot,
    zero_expost_q = ifelse(q_at == 0, 1, 0),
    zero_diff_q = ifelse(diff_q == 0, 1, 0)
  )

item_chars_df %>%
  ggplot(aes(x = diff_q)) +
  geom_histogram() +
  scale_x_continuous(labels = scales::percent) +
  labs(
    x = TeX("%$\\Delta$ Quantity by Item ID"),
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

saveLastFig("fig2a")
## End of Fig2a ##

## Figure 2b ##
diff_q_df <- item_chars_df %>%
  mutate(
    diff_q = (q_at - q_ot) / q_ot
  ) %>%
  group_by(item_id_sequential) %>%
  summarize(
    num_auctions = n(),
    mean_qa = mean(q_at),
    mean_qo = mean(q_ot),
    mean_diff_q = mean(diff_q),
    median_diff_q = median(diff_q),
    sd_diff_q = sd(diff_q)
  ) %>%
  mutate(
    zero_mean_diff_q = ifelse(mean_diff_q == 0, 1, 0)
  )

diff_q_df %>%
  arrange(
    sd_diff_q
  ) %>%
  ggplot(aes(x = mean_diff_q, y = sd_diff_q)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = TeX("Mean %$\\Delta$ Quantity"),
    y = TeX("Standard Deviation %$\\Delta$ Quantity")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
saveLastFig("fig2b")
## End of Fig2b ##
