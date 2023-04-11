##==================================================
##
## Script name: Tables and Figures 2. Reduced Form
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script:  Create data description tables and figures
##
## Input: data/estimation_step1_output_minfit.rdata
##
## Output: outputs/reduced_form/
##         outputs/reduced_form/fig3a.jpg
##         outputs/reduced_form/fig3b.jpg
##         outputs/reduced_form/fig4a.jpg
##         outputs/reduced_form/fig4b.jpg
##         outputs/reduced_form/app_fig10a.jpg
##         outputs/reduced_form/app_fig10b.jpg
##         outputs/reduced_form/app_fig11a.jpg
##         outputs/reduced_form/app_fig11b.jpg
##         outputs/reduced_form/app_fig13.jpg
##         outputs/reduced_form/app_fig14.jpg
##
##==================================================

## Libraries ## 
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(RColorBrewer)
library(devtools)
library(lfe)
library(mltools)

# Create .library if it doesn't exist
if (!dir.exists("presentation/.library")) {
  dir.create("presentation/.library")
}

# Check if binscattr is installed
if (!requireNamespace("binscattr", lib.loc="presentation/.library/")) {
  # Tell the user we're installing binscattr
  message("Installing binscattr to presentation/.library/")

  # If not, install it
  devtools::install_local(
    "presentation/binscattr/binscattr/", 
    upgrade="ask", 
    force=TRUE,
    lib="presentation/.library/",
    build_vignettes = FALSE
  )
}

# Load binscattr
library(binscattr, lib.loc="presentation/.library/")

## To skip annoying warnings if just running through to replicate
options(warn=-1)

theme_set(theme_minimal())

## Load the data
### 1st stage estimates + transformed input data
load(file.path("data","estimation_step1_output_minfit.rdata"))

## directory names for outputs
output_dir = file.path("outputs","reduced_form")
dir.create(output_dir)

saveLastFig <- function(fname){
  fpath = file.path(output_dir, paste0(fname, ".jpg"))
  ggsave(fpath)
}


## Load Data ##
qa_model_df <- data.frame(
  contract_no = demo_project_item_df$contract_no,
  old_item_id = demo_project_item_df$old_item_id,
  km_cluster = demo_project_item_df$km_cluster
) %>%
  mutate(
    q_at_model = qa_model_fit,
    sigma_t = sigma_t_fit,
    sigma_t_sq = sigma_t * sigma_t
  )

more_project_chars <- bidder_project_level_df %>%
  select(
    project_bidder_id,
    designed_inhouse,
    office_score_estimate
  ) %>%
  unique()

project_chars <- empirical_bid_data %>%
  select(
    project_bidder_id,
    project_id_sequential,
    contract_no,
    bidder_specialization,
    fringe,
    utilization,
    same_district,
    proj_duration,
    num_items_not_used,
    tot_top_skew_item,
    project_manager_overrun_share,
    project_manager_underrun_share,
    resident_engineer_overrun_share,
    resident_engineer_underrun_share,
    state_100,
    bid_open_year,
    project_type_id,
    designer_id,
    proj_manager_id,
    engineer_id,
    num_items,
    num_bidders
  ) %>%
  mutate(
    designer_id = as.numeric(factor(designer_id))
  ) %>%
  group_by(project_bidder_id) %>%
  summarize_all(first) %>%
  ungroup() %>%
  left_join(more_project_chars, by = "project_bidder_id") %>%
  select(-project_bidder_id,
         -project_id_sequential) %>%
  group_by(contract_no) %>%
  summarize_all(mean) %>% ungroup()


bridge_empirical_bid_data <- empirical_bid_data %>%
  select(project_bidder_id,
         contract_no,
         project_id_sequential,
         project_type_id,
         item_id_sequential,
         bidder_id_sequential,
         bridge,
         bid_unit_price,
         office_unit_price,
         old_item_id,
         q_at,
         q_ot,
         num_bidders,
         rank,
         extra_work_payment,
         item_description) %>%
  dplyr::filter(bridge == 1) %>%
  left_join(
    qa_model_df, by = c("contract_no","old_item_id")
  )

### Check for symmetry across bidders
graph_data_full <- bridge_empirical_bid_data %>%
  filter(rank < 3) %>%
  select(
    contract_no,
    item_id_sequential,
    item_description,
    bid_unit_price,
    office_unit_price,
    q_at,
    q_ot,
    q_at_model,
    sigma_t,
    rank
  ) %>%
  mutate(
    delta_bid = (bid_unit_price - office_unit_price)/office_unit_price,
    delta_q = (q_at - q_ot)/q_ot
  ) %>%
  group_by(contract_no) %>%
  mutate(
    bidder_total = sum(bid_unit_price * q_at),
    office_total = sum(office_unit_price * q_ot),
    bidder_total_exante = sum(bid_unit_price * q_ot)
  ) %>%
  ungroup() %>%
  mutate(
    bid_weight_exante = (q_ot*bid_unit_price)/bidder_total_exante,
    bid_weight_expost = (q_at * bid_unit_price)/bidder_total,
    estimate_cost_weight = (q_ot * office_unit_price)/office_total
  ) %>%
  left_join(project_chars)

ptiles_deltabid = quantile(graph_data_full$delta_bid, probs=seq(0,1,0.01))
ptiles_deltaq = quantile(graph_data_full$delta_q, probs=seq(0,1,0.01))
ptiles_sigmat = quantile(graph_data_full$sigma_t, probs=seq(0,1,0.01))

myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(graph_data_full$rank)
colScale <- scale_colour_manual(name = "Rank",values = myColors)

graph_data_full %>%
  dplyr::filter(
    delta_bid > ptiles_deltabid[2] & delta_bid < ptiles_deltabid[99]
  ) %>%
  dplyr::filter(delta_q < ptiles_deltaq[99]) %>%
  binscatter( y=delta_bid, x =delta_q,
              grouping_var = rank,
              # pos="",
              controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
  ) +
  scale_x_continuous(labels =scales::percent ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('%$\\Delta$ Bid $t$'),
    x =  TeX('%$\\Delta$ Quantity $t$')
  ) +
  theme_minimal() +
  colScale +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )
saveLastFig("fig3a")
## End of Fig 3a ##

### Fig 3b ###
graph_data_compare <- graph_data_full %>%
  select(
    contract_no,
    item_id_sequential,
    rank,
    bid_unit_price,
    office_unit_price,
    delta_q
  ) %>%
  spread(
    key = rank,
    value = bid_unit_price
  ) %>%
  rename(
    winner_bid = `1`,
    loser_bid = `2`
  ) %>%
  mutate(
    winner_delta_bid = (winner_bid - office_unit_price)/office_unit_price,
    loser_delta_bid = (loser_bid - office_unit_price)/office_unit_price
  ) %>%
  left_join(project_chars)

ptiles_deltabid = quantile(graph_data_full$delta_bid, probs=seq(0,1,0.01))
ptiles_deltaq = quantile(graph_data_full$delta_q, probs=seq(0,1,0.01))
ptiles_sigmat = quantile(graph_data_full$sigma_t, probs=seq(0,1,0.01))

ptiles_deltabid_winner = quantile(graph_data_compare$winner_delta_bid, probs=seq(0,1,0.01))
ptiles_deltabid_loser = quantile(graph_data_compare$loser_delta_bid, probs=seq(0,1,0.01))


trunc_graph_data_compare <-
  graph_data_compare %>%
  dplyr::filter(
    winner_delta_bid > ptiles_deltabid_winner[2] & winner_delta_bid < ptiles_deltabid_winner[99]
  ) %>%
  dplyr::filter(
    loser_delta_bid > ptiles_deltabid_loser[2] & loser_delta_bid < ptiles_deltabid_loser[99]
  ) %>%
  dplyr::filter( delta_q < ptiles_deltaq[99])

trunc_graph_data_compare %>%
  binscatter_manual( x=winner_delta_bid, y =loser_delta_bid, pos = "",
                     controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
  ) +
  scale_x_continuous(labels =scales::percent ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('Second Place %$\\Delta$ Bid'),
    x =  TeX('Winner %$\\Delta$ Bid')
  ) + theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )

saveLastFig("fig3b")
## End of fig 3b ##

## Fig 4a ##
graph_data <- bridge_empirical_bid_data %>%
  filter(rank==1) %>%
  select(
    contract_no,
    item_id_sequential,
    item_description,
    bid_unit_price,
    office_unit_price,
    q_at,
    q_ot,
    q_at_model,
    sigma_t
  ) %>%
  group_by(contract_no) %>%
  mutate(
    bidder_total = sum(bid_unit_price * q_at),
    office_total = sum(office_unit_price * q_ot),
    bidder_total_exante = sum(bid_unit_price * q_ot)
  ) %>%
  ungroup() %>%
  mutate(
    delta_bid = (bid_unit_price - office_unit_price)/office_unit_price,
    delta_q = (q_at - q_ot)/q_ot,
    delta_bid_weight = ( (bid_unit_price * q_at / bidder_total) - (office_unit_price * q_ot / office_total))/ (office_unit_price * q_ot / office_total),
    delta_bid_weight_model = ( (bid_unit_price * q_at_model / bidder_total) - (office_unit_price * q_ot / office_total))/ (office_unit_price * q_ot / office_total),
    bid_weight_exante = (q_ot*bid_unit_price)/bidder_total_exante,
    bid_weight_expost = (q_at * bid_unit_price)/bidder_total
  ) %>%
  left_join(project_chars)

graph_data %>%
  dplyr::filter(
    delta_bid > ptiles_deltabid[2] & delta_bid < ptiles_deltabid[99]
  ) %>%
  dplyr::filter(sigma_t > ptiles_sigmat[2] & sigma_t < ptiles_sigmat[99]) %>%
  mutate(
    abs_delta_bid = sqrt((delta_bid)^2)
  ) %>%
  binscatter_manual( y=abs_delta_bid, x =sigma_t,
                     # pos="top right",
                     pos="none",
                     controls=c("delta_q","project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
  ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('| %$\\Delta$ Bid $t$ |'),
    x =  TeX('Item Quantity Standard Deviation')
  ) + theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )

saveLastFig("fig4a")


## Fig 4b ##
graph_data %>%
  dplyr::filter(
    delta_bid > ptiles_deltabid[2] & delta_bid < ptiles_deltabid[99]
  ) %>%
  dplyr::filter(sigma_t > ptiles_sigmat[2] & sigma_t < ptiles_sigmat[99]) %>%
  binscatter_manual( y=delta_bid_weight, x =sigma_t,
                     controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential"),
                     # pos = c("top right")
                     pos = "NA"
  ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    x = TeX('Item Quantity Standard Deviation'),
    y = TeX('%$\\Delta$ Proportion of Revenue from Item $t$')
  ) +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )

saveLastFig("fig4b")

### Appendix Fig 11a: compare top 2 bidders when they are close ######
close_bidders <- graph_data_full %>%
  group_by(contract_no, rank) %>%
  summarize(
    total = sum(bid_unit_price * q_ot)
  ) %>%
  ungroup() %>%
  mutate(
    winner_total = ifelse(rank==1, total, 0)
  ) %>%
  group_by(contract_no) %>%
  mutate(
    winner_total = max(winner_total),
    total_diff = (total-winner_total)/winner_total,
    keep = ifelse(rank == 2 & (total_diff  < 0.1), 1, 0)
  ) %>%
  ungroup() %>%
  filter(keep == 1)

graph_data_close_bidders <- graph_data_full %>%
  filter(
    contract_no %in% close_bidders$contract_no
  )

ptiles_deltabid = quantile(graph_data_close_bidders$delta_bid, probs=seq(0,1,0.01))
ptiles_deltaq = quantile(graph_data_close_bidders$delta_q, probs=seq(0,1,0.01))
ptiles_sigmat = quantile(graph_data_close_bidders$sigma_t, probs=seq(0,1,0.01))

myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(graph_data_close_bidders$rank)
colScale <- scale_colour_manual(name = "Rank",values = myColors)

graph_data_close_bidders %>%
  dplyr::filter(
    delta_bid > ptiles_deltabid[2] & delta_bid < ptiles_deltabid[99]
  ) %>%
  dplyr::filter(delta_q < ptiles_deltaq[99]) %>%
  binscatter_manual_by_group( y=delta_bid, x =delta_q,
                              grouping_var = rank,
                              # pos="",
                              controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
  ) +
  scale_x_continuous(labels =scales::percent ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('%$\\Delta$ Bid $t$'),
    x =  TeX('%$\\Delta$ Quantity $t$')
  ) + theme_minimal() +
  colScale +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )
saveLastFig("app_fig11a")


## Appendix Fig 11b  ##
graph_data <- bridge_empirical_bid_data %>%
  filter(rank==1) %>%
  select(
    contract_no,
    item_id_sequential,
    item_description,
    bid_unit_price,
    office_unit_price,
    q_at,
    q_ot,
    q_at_model,
    sigma_t
  ) %>%
  group_by(contract_no) %>%
  mutate(
    bidder_total = sum(bid_unit_price * q_at),
    office_total = sum(office_unit_price * q_ot),
    bidder_total_exante = sum(bid_unit_price * q_ot)
  ) %>%
  ungroup() %>%
  mutate(
    delta_bid = (bid_unit_price - office_unit_price)/office_unit_price,
    delta_q = (q_at - q_ot)/q_ot,
    delta_bid_weight = ( (bid_unit_price * q_at / bidder_total) - (office_unit_price * q_ot / office_total))/ (office_unit_price * q_ot / office_total),
    delta_bid_weight_model = ( (bid_unit_price * q_at_model / bidder_total) - (office_unit_price * q_ot / office_total))/ (office_unit_price * q_ot / office_total),
    bid_weight_exante = (q_ot*bid_unit_price)/bidder_total_exante,
    bid_weight_expost = (q_at * bid_unit_price)/bidder_total
  ) %>%
  left_join(project_chars)

ptiles_deltabid = quantile(graph_data_full$delta_bid, probs=seq(0,1,0.01))
ptiles_deltaq = quantile(graph_data_full$delta_q, probs=seq(0,1,0.01))
ptiles_sigmat = quantile(graph_data_full$sigma_t, probs=seq(0,1,0.01))

graph_data %>%
  dplyr::filter(
    delta_bid > ptiles_deltabid[2] & delta_bid < ptiles_deltabid[99]
  ) %>%
  dplyr::filter(sigma_t > ptiles_sigmat[2] & sigma_t < ptiles_sigmat[99]) %>%
  mutate(
    abs_delta_bid = sqrt((delta_bid)^2)
  ) %>%
  binscatter_manual( y=abs_delta_bid, x =sigma_t,
                     pos="",
                     controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
  ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('| %$\\Delta$ Bid $t$ |'),
    x =  TeX('Item Quantity Standard Deviation')
  ) + theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )

saveLastFig("app_fig11b")

### Appendix Fig 10a: compare the top 4 bidders ######
graph_data_compare4 <- bridge_empirical_bid_data %>%
  filter(rank < 5) %>%
  select(
    contract_no,
    item_id_sequential,
    item_description,
    bid_unit_price,
    office_unit_price,
    q_at,
    q_ot,
    q_at_model,
    sigma_t,
    rank
  ) %>%
  mutate(
    delta_bid = (bid_unit_price - office_unit_price)/office_unit_price,
    delta_q = (q_at - q_ot)/q_ot
  ) %>%
  group_by(contract_no) %>%
  mutate(
    bidder_total = sum(bid_unit_price * q_at),
    office_total = sum(office_unit_price * q_ot),
    bidder_total_exante = sum(bid_unit_price * q_ot)
  ) %>%
  ungroup() %>%
  mutate(
    bid_weight_exante = (q_ot*bid_unit_price)/bidder_total_exante,
    bid_weight_expost = (q_at * bid_unit_price)/bidder_total,
    estimate_cost_weight = (q_ot * office_unit_price)/office_total
  ) %>%
  left_join(project_chars)

ptiles_deltabid = quantile(graph_data_compare4$delta_bid, probs=seq(0,1,0.01))
ptiles_deltaq = quantile(graph_data_compare4$delta_q, probs=seq(0,1,0.01))
ptiles_sigmat = quantile(graph_data_compare4$sigma_t, probs=seq(0,1,0.01))

myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(graph_data_compare4$rank)
colScale <- scale_colour_manual(name = "Rank",values = myColors)

graph_data_compare4 %>%
  dplyr::filter(
    delta_bid > ptiles_deltabid[2] & delta_bid < ptiles_deltabid[99]
  ) %>%
  dplyr::filter(delta_q < ptiles_deltaq[99]) %>%
  binscatter( y=delta_bid, x =delta_q,
              grouping_var = rank,
              # pos="",
              controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
  ) +
  scale_x_continuous(labels =scales::percent ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('%$\\Delta$ Bid $t$'),
    x =  TeX('%$\\Delta$ Quantity $t$')
  ) +
  theme_minimal() +
  colScale +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  )

saveLastFig("app_fig10a")

## End of Fig 10a ##

### Appendix Fig 10b ###
graph_data_compare4b <- graph_data_compare4 %>%
  select(
    contract_no,
    item_id_sequential,
    rank,
    bid_unit_price,
    office_unit_price,
    delta_q
  ) %>%
  spread(
    key = rank,
    value = bid_unit_price
  ) %>%
  rename(
    winner_bid = `1`,
    rank2_bid = `2`,
    rank3_bid = `3`,
    rank4_bid = `4`
  ) %>%
  mutate(
    winner_delta_bid = (winner_bid - office_unit_price)/office_unit_price,
    rank2_delta_bid = (rank2_bid - office_unit_price)/office_unit_price,
    rank3_delta_bid = (rank3_bid - office_unit_price)/office_unit_price,
    rank4_delta_bid = (rank4_bid - office_unit_price)/office_unit_price
  ) %>%
  left_join(project_chars)

ptiles_deltabid = quantile(graph_data_compare4$delta_bid, probs=seq(0,1,0.01))
ptiles_deltaq = quantile(graph_data_compare4$delta_q, probs=seq(0,1,0.01))
ptiles_sigmat = quantile(graph_data_compare4$sigma_t, probs=seq(0,1,0.01))

ptiles_deltabid_winner = quantile(graph_data_compare4b$winner_delta_bid, probs=seq(0,1,0.01))
ptiles_deltabid_rank2 = quantile(graph_data_compare4b$rank2_delta_bid, probs=seq(0,1,0.01), na.rm=T)
ptiles_deltabid_rank3 = quantile(graph_data_compare4b$rank3_delta_bid, probs=seq(0,1,0.01), na.rm=T)
ptiles_deltabid_rank4 = quantile(graph_data_compare4b$rank4_delta_bid, probs=seq(0,1,0.01), na.rm=T)


trunc_graph_data_compare4 <-
  graph_data_compare4b %>%
  dplyr::filter(
    winner_delta_bid > ptiles_deltabid_winner[2] & winner_delta_bid < ptiles_deltabid_winner[99]
  ) %>%
  dplyr::filter(
    rank2_delta_bid > ptiles_deltabid_rank2[2] & rank2_delta_bid < ptiles_deltabid_rank2[99]
  ) %>%
  dplyr::filter(
    rank3_delta_bid > ptiles_deltabid_rank3[2] & rank3_delta_bid < ptiles_deltabid_rank3[99]
  ) %>%
  dplyr::filter(
    rank4_delta_bid > ptiles_deltabid_rank4[2] & rank4_delta_bid < ptiles_deltabid_rank4[99]
  ) %>%
  dplyr::filter( delta_q < ptiles_deltaq[99])

trunc_graph_data_compare4 <- trunc_graph_data_compare4 %>%
  rename(
    `Rank 2` = rank2_delta_bid,
    `Rank 3` = rank3_delta_bid,
    `Rank 4` = rank4_delta_bid
  ) %>%
  pivot_longer(
    cols = contains("Rank "),
    names_to = "rank",
    values_to = "loser_delta_bid"
  ) %>%
  mutate(
    rank_stripped = str_replace(rank, "Rank ", "")
  )

trunc_graph_data_compare4 %>%
  binscatter( x=winner_delta_bid, y = loser_delta_bid, grouping_var = rank_stripped,
                     controls=c("project_type_id","office_score_estimate", "bid_open_year", "proj_duration", "proj_manager_id", "designer_id", "engineer_id","num_items","num_bidders","item_id_sequential")
                     # bins = 20
  ) +
  theme_minimal() +
  scale_colour_manual(name = "Rank",values = myColors) +
  scale_x_continuous(labels =scales::percent ) +
  scale_y_continuous(labels =scales::percent ) +
  labs(
    y =  TeX('Losing %$\\Delta$ Bid'),
    x =  TeX('Winner %$\\Delta$ Bid')
    ) +
  theme(
        text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10),)
  )


saveLastFig("app_fig10b")


## Extra: Deep dive into the items underlying Fig 4a ##
## Online Appendix Fig 13 + Fig 14 ##
inspect_data = graph_data %>%
  arrange(sigma_t) %>%
  select(
    q_ot,
    q_at,
    sigma_t,
    item_id_sequential,
    tot_top_skew_item,
    item_description,
    office_unit_price,
    delta_bid,
    delta_bid_weight,
    delta_q,
    contract_no,
    designer_id,
    project_manager_overrun_share,
    project_manager_underrun_share
  )

quantile(inspect_data$sigma_t, seq(0,1,.1))

raw_most_overunning_items = inspect_data %>%
  filter(delta_q > quantile(inspect_data$delta_q, 0.95)) %>%
  select(item_description,delta_q) %>%
  # unique() %>%
  mutate(
    item_description = str_to_lower(item_description)
  )


raw_most_variable_items = inspect_data %>%
  filter(sigma_t > quantile(inspect_data$sigma_t, 0.95)) %>%
  select(item_description) %>%
  # unique() %>%
  mutate(
    item_description = str_to_lower(item_description)
  )

processed_most_variable_items = inspect_data %>%
  filter(sigma_t < quantile(inspect_data$sigma_t, 0.05)) %>%
  select(item_description) %>%
  # unique() %>%
  mutate(
    item_description = str_to_lower(item_description),
    item_description = str_replace_all(item_description, "[a-z]-(.*)|bridge #(.*)|bridge no.(.*)|psi|/ft./|/in./|mm|br. no.*|br no.*|pier(.*)|location(.*)", " "),
    item_description = str_replace_all(item_description, "[0-9]|[:punct:]|[:blank:]of[:blank:]|inch|millimeter|[:blank:]mm[:blank:]|[:blank:]kg[:blank:]", " "),
    item_description = str_replace_all(item_description, "[:blank:][a-z][:blank:]|[:blank:][a-z]$|[:blank:]no", " "),
    item_description = str_squish(item_description)
  )

least_variable_items = inspect_data %>%
  filter(sigma_t < quantile(inspect_data$sigma_t, 0.05)) %>%
  select(item_description) %>%
  # unique() %>%
  mutate(
    item_description = str_to_lower(item_description),
    item_description = str_replace_all(item_description, "[a-z]-(.*)|bridge #(.*)|bridge no.(.*)|psi|/ft./|/in./|mm|br. no.*|br no.*|pier(.*)|location(.*)", " "),
    item_description = str_replace_all(item_description, "[0-9]|[:punct:]|[:blank:]of[:blank:]|inch|millimeter|[:blank:]mm[:blank:]|[:blank:]kg[:blank:]|[:blank:]mpa[:blank:]", " "),
    item_description = str_replace_all(item_description, "[:blank:][a-z][:blank:]|[:blank:][a-z]$|[:blank:]no", " "),
    item_description = str_squish(item_description)
  ) %>%
  count(item_description, sort = TRUE) %>%
  filter(item_description!="temporary") ## This refers to "temporary bridge no [code]" which is not interpretable


least_variable_items %>%
  dplyr::filter(n > 10) %>%
  mutate(item_description = reorder(item_description, n)) %>%
  ggplot(aes(x = item_description, y = n)) + geom_col() +
  coord_flip() + 
  labs(
    y = "Frequency Among Bottom 5% Standard Deviation Instances",
    x = "Trimmed Item Description"
  )

saveLastFig("app_fig13")

most_variable_items = inspect_data %>%
  filter(sigma_t > quantile(inspect_data$sigma_t, 0.95)) %>%
  select(item_description) %>%
  mutate(
    item_description = str_to_lower(item_description),
    item_description = str_replace_all(item_description, "[a-z]-(.*)|bridge #(.*)|bridge no.(.*)|psi|/ft./|/in./|mm|br. no.*|br no.*|pier(.*)|location(.*)", " "),
    item_description = str_replace_all(item_description, "[0-9]|[:punct:]|[:blank:]of[:blank:]|inch|millimeter|[:blank:]mm[:blank:]|[:blank:]kg[:blank:]|[:blank:]mpa[:blank:]", " "),
    item_description = str_replace_all(item_description, "\\+rte|[:blank:][a-z][:blank:]|[:blank:][a-z]$|[:blank:]no", " "),
    item_description = str_squish(item_description)
  ) %>%
  count(item_description, sort = TRUE)


most_variable_items %>%
  filter(n > 10) %>%
  mutate(item_description = reorder(item_description, n)) %>%
  ggplot(aes(x = item_description, y = n)) + geom_col() +
  coord_flip() + 
  labs(
    y = "Frequency Among Top 5% Standard Deviation Instances",
    x = "Trimmed Item Description"
  )

saveLastFig("app_fig14")

## End Online Appendix ##
