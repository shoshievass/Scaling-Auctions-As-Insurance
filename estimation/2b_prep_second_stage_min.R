##==================================================
##
## Script name: Prep Bid and Quantity Data for Type Estimation in Julia 
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script: Export bid and quantity model data to julia for gmm 
##
## Author: Shoshana Vasserman
##
## Date Last Edited: 2022-06-01
##
## Email: svass@stanford.edu
##
## Input:  Data/Intermediate/normal_qamodel_bridgesmscale_estimation_stable.rdata
##
##
## Output: 

## Packages
library(tidyverse);library(rstan)

load("replication_package/data/estimation_step2_inputs/estimation_step1_output_minfit.rdata")

## Directories
gmm_input_dirname <- "replication_package/data/"
output_folder_name <- 'CARA_Estimation_Input_test_Dec2022'
gmm_inputs_dir = paste0(gmm_input_dirname,output_folder_name,"/")
dir.create(gmm_inputs_dir)


## Get highest item variation in qa vs qo from data
item_variation_df <- demo_project_item_df %>%
  select(item_id_sequential,
         q_ot,
         q_at,
         top_skew_item) %>%
  group_by(
    item_id_sequential
  ) %>%
  mutate(
    abs_q_diff = abs(q_at - q_ot)/q_ot
  ) %>%
  summarize(
    mean_abs_q_diff  = mean(abs_q_diff),
    median_abs_q_diff = median(abs_q_diff),
    top_skew_item = first(top_skew_item)
  )

## Get most common items
item_frequency_df <- demo_project_item_df %>%
  select(item_id_sequential,
         q_ot,
         q_at,
         top_skew_item) %>%
  group_by(
    item_id_sequential
  ) %>%
  summarize(
    item_frequency = n()
    ) %>%
  ungroup()

item_frequency_quantiles = quantile(item_frequency_df$item_frequency, probs = seq(0,1,0.025))

unit_item_df <- demo_project_item_df %>%
  select(item_id_sequential,
         q_ot,
         q_at,
         top_skew_item) %>%
  mutate(
    unit_obs = ifelse(q_at == 1 & q_ot == 1, 1, 0)
  ) %>%
  group_by(item_id_sequential) %>%
  summarize(
    num_obs = n(),
    prop_units = mean(unit_obs)
  ) %>%
  mutate(
    unit_only = 0
  ) %>%
  select(item_id_sequential, unit_only)


demo_project_item_df <- demo_project_item_df %>%
  left_join(unit_item_df, by = c("item_id_sequential")) %>%
  left_join(item_frequency_df, by = c("item_id_sequential")) %>%
  mutate(
    skew_cluster = ifelse(top_skew_item == 1, 1, 2),
    frequency_cluster = ifelse(item_frequency > item_frequency_quantiles[length(item_frequency_quantiles)-1], 1, 2)
  )

examine_item_freqs <- demo_project_item_df %>%
  select(
    item_id_sequential,
    top_skew_item,
    item_frequency,
    frequency_cluster
    ) %>%
  distinct()


table(demo_project_item_df$skew_cluster)
table(demo_project_item_df$frequency_cluster)


item_features <- demo_project_item_df %>%
  select(
    contract_no,
    old_item_id
  ) %>%
  bind_cols(as.data.frame(X_q))

sigma_t_quants <- quantile(sigma_t_fit,probs=seq(0,1,.1))

qa_moodel_df <- data.frame(
  contract_no = demo_project_item_df$contract_no,
  old_item_id = demo_project_item_df$old_item_id,
  km_cluster = demo_project_item_df$km_cluster,
  skew_cluster = demo_project_item_df$skew_cluster,
  frequency_cluster = demo_project_item_df$frequency_cluster,
  unit_only_dummy = demo_project_item_df$unit_only
  ) %>%
  mutate(
    q_at_model = qa_model_fit,
    sigma_t = sigma_t_fit
    )

firm_df <- bidder_project_level_df%>%
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

empirical_bid_data_t <- empirical_bid_data %>%
  left_join(
    qa_moodel_df, by = c("contract_no","old_item_id")
  )

empirical_bid_data_t <- empirical_bid_data_t %>%
  left_join(
    firm_df, by = c("project_bidder_id")
  )


## Examine project size
project_sizes <- bidder_project_level_df %>%
  arrange(project_id_seq, bidder_score) %>%
  group_by(project_id_seq) %>%
  summarize_all(first) %>%
  select(project_id_seq,
         office_score_estimate,
         bidder_score,
         office_cost_dot_expost_quantity,
         bid_dot_expost_quantity,
         tot_top_skew_item) %>%
  ungroup() %>%
  arrange(office_score_estimate)

score_est_quants <- quantile(project_sizes$office_score_estimate, probs = seq(0,1,0.005))

## Notice two outlier aucs -- will exclude them
project_sizes %>% ggplot(aes(x = office_score_estimate)) + geom_histogram()

big_projects <- project_sizes$project_id_seq[project_sizes$office_score_estimate > score_est_quants[length(score_est_quants)- 1]]
tiny_projects <- project_sizes$project_id_seq[project_sizes$office_score_estimate < score_est_quants[2]]

empirical_bid_data %>%
  filter(project_id_sequential %in% big_projects) %>%
  select(contract_no) %>%
  distinct

empirical_bid_data %>%
  filter(project_id_sequential %in% tiny_projects) %>%
  select(contract_no) %>%
  distinct

format_numeric <- function(x) {
  numeric_cols <- vapply(x, is.numeric, logical(1))
  x[numeric_cols] <- lapply(x[numeric_cols], format, scientific = FALSE)
  x
}

more_project_chars <- bidder_project_level_df %>%
  select(
    project_bidder_id,
    designed_inhouse,
    office_score_estimate
  ) %>%
  unique()

bidder_project_chars <- empirical_bid_data %>%
  left_join(
    firm_df, by = c("project_bidder_id")
  ) %>%
  select(
    bidder_id_sequential,
    project_bidder_id,
    project_id_sequential,
    contract_no,
    bidder_specialization,
    # fringe,
    capacity,
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
    fringe_num_aucs
  ) %>%
  mutate(
    designer_id = as.numeric(factor(designer_id))
  ) %>%
  group_by(project_bidder_id) %>%
  summarize_all(first) %>%
  # ungroup() %>%
  left_join(more_project_chars, by = "project_bidder_id") %>%
  ungroup()

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

bidder_type_X_mat <- bidder_project_chars %>%
  select(-fringe_num_aucs, -contract_no, -project_type_id, -designer_id, -proj_manager_id, -bid_open_year, -engineer_id, -project_bidder_id, -project_id_sequential,-bidder_id_sequential) %>%
  as.matrix %>%
  scale(.)

bidder_type_X_df <-
  data.frame(
    contract_no = bidder_project_chars$contract_no,
    bidder_id = bidder_project_chars$bidder_id_sequential,
    fringe_num_aucs = bidder_project_chars$fringe_num_aucs) %>%
  bind_cols(as.data.frame(bidder_type_X_mat))

bidder_type_X_df_scaled <- bidder_type_X_df%>%
  mutate_at(vars(-bidder_id, -contract_no, -fringe_num_aucs), scale)


project_mean_bidder_type_X_df <- bidder_type_X_df %>%
  group_by(contract_no) %>%
  mutate_at(vars(-bidder_id, -contract_no), "mean") %>%
  ungroup()
# save(project_mean_bidder_type_X_df, file = "data/project_mean_bidder_type_X_df_2020-04-14.rdata")
project_mean_bidder_type_X_df_scaled <- project_mean_bidder_type_X_df %>%
  mutate_at(vars(-bidder_id, -contract_no), scale)


# library(binscattr)
# demo_project_item_df$qa_model_fit = qa_model_fit
# binscatter(data = demo_project_item_df, x = q_at, y = qa_model_fit)

getSampleData <- function(sample_demo_df){

  sample_bidder_ids <- unique(sample_demo_df$bidder_id_sequential)

  sample_demo_df <- sample_demo_df %>%
    filter(unit_only_dummy == 0 )

  auction_df <- sample_demo_df %>%
    filter(rank == 1)  %>%
  select(
    # project_bidder_id,
    # bidder_id_sequential,
    contract_no,
    project_type_id,
    q_at,
    q_at_model,
    sigma_t,
    q_ot,
    # bid_unit_price,
    office_unit_price,
    # hazard_rate,
    bidder_auction_mean,
    bidder_auction_sigma_k,
    item_id_sequential,
    # item_type_group_sequential,
    item_unit_sequential,
    old_item_id,
    # km_cluster,
    skew_cluster,
    frequency_cluster,
    unit_only_dummy,
    # random_cluster,
    extra_work_payment
  ) %>%
    mutate(
      # q_at_model = qa_model_fit,
      # sigma_t = sigma_t_fit,
      item_id_sequential = format(item_id_sequential, scientific=F),
      # item_km_cliuster = demo_project_item_df,
      item_unit_sequential = format(item_unit_sequential, scientific = F)
    )
    # format_csv(format_numeric(.)) %>%

  auction_df %>% write_csv(paste0(gmm_input_dirname,output_folder_name,"/sample_project_",sample_demo_df$contract_no[1],".csv"))
  
  bidder_df <- sample_demo_df %>%
    # filter(rank < 5 ) %>%
    select(bidder_id_sequential, bid_unit_price, item_id_sequential) %>%
    mutate(
      item_id_sequential = format(item_id_sequential, scientific=F)
    ) %>%
    spread(bidder_id_sequential, bid_unit_price)

  auction_df %>%
    # select(item_id_sequential,frequency_cluster) %>%
    select(item_id_sequential,skew_cluster) %>%
    left_join(bidder_df) %>%
    write_csv(paste0(gmm_input_dirname,output_folder_name,"/sample_project_bidders_",sample_demo_df$contract_no[1],".csv"))

  # biddertype_df <- bidder_type_X_df_scaled %>%
  #   filter(contract_no == auction_df$contract_no[1])
  
  biddertype_df <- bidder_type_X_df %>%
    filter(contract_no == auction_df$contract_no[1])
  # 
  biddertype_df %>% write_csv(paste0(gmm_input_dirname,output_folder_name,"/sample_project_biddertypes_",sample_demo_df$contract_no[1],".csv"))
  
  
  # bidder_aggtype_df <- project_mean_bidder_type_X_df_scaled %>%
  #   filter(contract_no == auction_df$contract_no[1])
  # 
  # bidder_aggtype_df %>% write_csv(paste0(gmm_input_dirname,output_folder_name,"/sample_project_aggbiddertypes_",sample_demo_df$contract_no[1],".csv"))

}

project_ids <- unique(empirical_bid_data$project_id_sequential)

contract_ids <- unique(empirical_bid_data$contract_no)
sink(paste0(gmm_input_dirname, "CF_Input_test_Dec2022_bridge_proj_ids.txt"), append=FALSE, split=FALSE)
cat("[", paste0(contract_ids, ","),"]")
sink()

sink(paste0(gmm_input_dirname,"CF_Input_test_Dec2022_bridge_proj_ids_parsed.txt"), append=FALSE, split=FALSE)
cat(contract_ids)
sink()

a <- empirical_bid_data_t %>%
filter(project_id_sequential %in% project_ids[1])
getSampleData(a)

empirical_bid_data_t %>%
  # filter(project_id_sequential %in% project_ids[1]) %>%
  filter(bridge==1) %>%
  filter(!(contract_no %in% c(63232, 71098))) %>%
  group_by(project_id_sequential) %>%
  do(getSampleData(.))

