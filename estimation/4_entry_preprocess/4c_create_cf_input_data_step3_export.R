library(tidyverse)
library(skimr)
library(ggridges)


## Last updated: May 26, 2022

## directory names for the counterfactuals pertaining to the same 2nd stage estimates
cf_input_dirname = paste0("data/estimation_step4_cf_inputs/")
cf_inputs_dir = paste0(cf_input_dirname,"/")
alpha_model_df <- read_csv(paste0(cf_inputs_dir,"alphas_by_auction_with_features.csv"))

## Load the dataz
## 1st stage estimates + transformed input data
gmm_input_1st_stage_data <- "estimation_step1_output_minfit.rdata"
load(paste0("data/",gmm_input_1st_stage_data))

w = 1000

## load mkp df
mkp_df <- read_csv("data/estimation_step3/second_stage_estimates_summary.csv")

###
gmm_output <- alpha_model_df %>%
  select(
    contract_no,
    bidder_id,
    alpha,
    gamma
  )

#### Export for Entry + Counterfactuals in Julia
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
         extra_work_payment) %>%
  dplyr::filter(bridge == 1) %>%
  left_join(
    qa_model_df, by = c("contract_no","old_item_id")
  )


bridge_empirical_bid_data <- bridge_empirical_bid_data %>%
  left_join(gmm_output, by= c("contract_no" = "contract_no", "bidder_id_sequential" = "bidder_id"))


bridge_empirical_bid_data <- bridge_empirical_bid_data %>%
  ungroup() %>%
  mutate(
    dollar_scale = w,
    risky_cost_term_mean = (office_unit_price/w * q_at_model)*(ifelse(rank==1,1,0)),
    risky_cost_term_variance_sans_gamma = ((office_unit_price/w)^2 * (0.5)*(sigma_t_sq))*(ifelse(rank==1,1,0)),
    risky_cost_term_correctqa_mean = (office_unit_price/w * q_at)*(ifelse(rank==1,1,0)),
    office_total_expost = (office_unit_price*q_at)*(ifelse(rank==1,1,0)),
    office_total_exante = (office_unit_price*q_ot)*(ifelse(rank==1,1,0))
  )
  

minimal_bridge_contract_data <- bridge_empirical_bid_data %>%
  group_by(contract_no) %>%
  summarize(
        dollar_scale = first(dollar_scale),
        extra_work_payment = first(extra_work_payment),
        risky_cost_term_mean = sum(risky_cost_term_mean),
        risky_cost_term_variance_sans_gamma = sum(risky_cost_term_variance_sans_gamma),
        risky_cost_term_correctqa_mean = sum(risky_cost_term_correctqa_mean),
        office_total_expost = sum(office_total_expost),
        office_total_exante = sum(office_total_exante),
        proj_type = first(project_type_id)
  )

test <- minimal_bridge_contract_data %>%
  select(
    contract_no,
    risky_cost_term_mean,
    risky_cost_term_correctqa_mean,
    risky_cost_term_variance_sans_gamma,
    office_total_expost,
    office_total_exante,
    proj_type
  )

minimal_bridge_contract_data_excludebig <- minimal_bridge_contract_data %>%
  filter(complete.cases(.))

type_distribution_df <- read_csv(paste0(cf_inputs_dir,"alpha_gamma_distribution.csv"))

### Save CF inputs ###

today = Sys.Date()
cf_data <- type_distribution_df %>%
  left_join(minimal_bridge_contract_data_excludebig) %>%
  relocate(
    contract_no,
    potential_bidder_bin_id,
    max_num_bidders,
    num_bids,
    lgalpha_mean,
    lgalpha_sd,
    alpha_bar_bayes,
    alpha_bar_bar,
    alpha_min,
    pois_gamma_alphacoef,
    pois_gamma_projfe,
    dollar_scale,
    extra_work_payment,
    office_total_exante,
    office_total_expost
  ) 

gmm_winner_mkp <- mkp_df %>%
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

winners_df <- bidder_project_level_df %>%
  left_join(id_map) %>%
  select(
    contract_no,
    bidder_id_seq,
    rank
  )%>% 
  left_join(gmm_winner_mkp, by= c("contract_no" = "contract_no", "bidder_id_seq" = "bidder_id")) %>%
  filter(complete.cases(.)) %>%
  filter(rank==1) %>%
  transmute(
    contract_no = contract_no,
    winning_alpha = alpha
  )

cf_data %>%
  left_join(winners_df) %>%
  mutate(winning_alpha_eval = pmin(winning_alpha, alpha_bar_bayes-1e-3)) %>%
  write_csv(paste0(cf_inputs_dir,"cfdata.csv"))



getSampleData <- function(sample_demo_df){

  sample_bidder_ids <- unique(sample_demo_df$bidder_id_sequential)

  sample_demo_df %>%
    filter(bidder_id_sequential == sample_bidder_ids[1]) %>%
    select(
      contract_no,
      q_at,
      q_at_model,
      sigma_t_sq,
      q_ot,
      office_unit_price,
      dollar_scale,
      extra_work_payment
    ) %>%
    write_csv(paste0(cf_inputs_dir,"sample_project_",sample_demo_df$contract_no[1],".csv"))


  sample_demo_df %>%
    select(bidder_id_sequential,bid_unit_price, item_id_sequential) %>%
    mutate(
      item_id_sequential = item_id_sequential
    ) %>%
    spread(bidder_id_sequential, bid_unit_price) %>%
    write_csv(paste0(cf_inputs_dir,"sample_project_bidders_",sample_demo_df$contract_no[1],".csv"))

}

project_ids <- unique(bridge_empirical_bid_data$project_id_sequential)
project_ids[2]

a <- bridge_empirical_bid_data %>%
  filter(project_id_sequential %in% project_ids[2])
getSampleData(a)

bridge_empirical_bid_data %>%
  group_by(project_id_sequential) %>%
  do(getSampleData(.))

contract_ids <- unique(bridge_empirical_bid_data$contract_no)
sink(paste0(cf_inputs_dir,"bridge_proj_ids_parsed.txt"), append=FALSE, split=FALSE)
cat(contract_ids)
sink()

