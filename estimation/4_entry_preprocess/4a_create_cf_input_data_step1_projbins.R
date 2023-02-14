##==================================================
##
## Script name: 4a_create_cf_input_data_step1_projbins.R
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script: Bin auctions into groups and prep alpha estimates for lognormal fit
##
## Input:  data/estimation_step1_output_minfit.rdata
##         data/estimation_step3/second_stage_estimates_summary.csv
##
## Output: data/estimation_step4_cf_inputs/potential_bidders_by_contract.csv
##         data/estimation_step4_cf_inputs/alphas_by_auction_with_features.csv
##

library(tidyverse)

## Load the data
### 1st stage estimates + transformed input data
load(file.path("data","estimation_step1_output_minfit.rdata"))
### 2nd stage estimates
gmm_output <- read_csv(file.path("data","estimation_step3","second_stage_estimates_summary.csv"))

## directory names for the counterfactuals pertaining to the same 2nd stage estimates
cf_inputs_dir = paste0("data/estimation_step4_cf_inputs/")
dir.create(cf_inputs_dir)

## Create project-level categories to figure out which bidders were plausibly around 
proj_level_categories <- empirical_bid_data %>% select(project_bidder_id,
                                                      contract_no,
                                                      bridge,
                                                      district, 
                                                      state_100, 
                                                      bid_open_year,
                                                      proj_manager_id,
                                                      engineer_id,
                                                      designer_id) %>%
  group_by(project_bidder_id) %>% 
  summarize_all(first) %>%
  mutate(
    bid_open_year_rounded = as.integer(bid_open_year),
    bid_open_year_rounded = ifelse(bid_open_year_rounded < 2001, 2000, bid_open_year_rounded),
    bid_open_year_rounded = ifelse(bid_open_year_rounded > 2012, 2013, bid_open_year_rounded),
    district_grouped = case_when(
      district %in% c(-1,1,2,3) ~ "1 or 2 or 3",
      district %in% c(4,5, 6) ~ "4 or 5 or 6",
    )
  ) %>%
  mutate(
    bid_open_year_grouped = as.integer(bid_open_year),
    bid_open_year_grouped = ifelse(bid_open_year_grouped < 2002, 2001, bid_open_year_grouped),
    bid_open_year_grouped = ifelse(bid_open_year_grouped > 2012, 2013, bid_open_year_grouped)
  )

bidder_project_level_descriptions <- bidder_project_level_df %>%
  left_join(proj_level_categories) %>%
  ungroup() 


## Get averaged bidder-project characteristics
proj_level_chars <- bidder_project_level_df %>%
  select(
    project_bidder_id,
    designed_inhouse,
    office_score_estimate
  ) %>%
  unique() %>%
  left_join(proj_level_categories, by = "project_bidder_id") %>%
  select(-project_bidder_id) %>%
  unique()

more_project_level_chars <- empirical_bid_data %>%
  select(
    contract_no,
    proj_duration,
    tot_top_skew_item,
    num_items_not_used,
    project_manager_overrun_share,
    project_manager_underrun_share,
    resident_engineer_overrun_share,
    resident_engineer_underrun_share
  ) %>%
  group_by(contract_no) %>% 
  summarize_all(first)

avg_bidder_project_chars <- empirical_bid_data %>%
  select(
    project_bidder_id,
    project_id_sequential,
    contract_no,
    bidder_specialization,
    capacity,
    utilization,
    same_district
  ) %>%
  group_by(project_bidder_id) %>%
  summarize_all(first) %>%
  ungroup() %>%
  select(-project_bidder_id) %>%
  group_by(contract_no, project_id_sequential) %>%
  summarize_all(mean) %>% ungroup()

alpha_model_df <- gmm_output %>%
  select(contract_no, alpha, gamma, bidder_id) %>%
  left_join(proj_level_chars, by = "contract_no") %>%
  left_join(more_project_level_chars, by = "contract_no") %>%
  left_join(avg_bidder_project_chars, by = "contract_no") %>%
  relocate(contract_no, alpha,gamma, bid_open_year_rounded, district_grouped, bid_open_year, district) 

write_csv(alpha_model_df, paste0(cf_inputs_dir,"alphas_by_auction_with_features.csv"))

## Get the set of potential bidders
num_unique_bidders_per_projtype <- bidder_project_level_descriptions %>%
  ungroup() %>%
  group_by(district_grouped, project_type_id, bid_open_year_grouped) %>%
  select(bidder_id_seq) %>%
  unique() %>%
  summarize(
    max_num_bidders__unique = n()
  ) %>%
  ungroup() %>%
  mutate(
    potential_bidder_bin_id = row_number()
  )

num_projects_per_projtype <- bidder_project_level_descriptions %>%
  ungroup() %>%
  group_by(district_grouped, project_type_id, bid_open_year_grouped) %>%
  select(project_id_seq) %>%
  unique() %>%
  summarize(
    num_unique_projects = n()
  )


num_bidders_seen_per_proj <- bidder_project_level_descriptions %>%
  ungroup() %>%
  group_by(contract_no) %>%
  summarize(
    num_bidders_seen = n()
  ) 


num_bidders_and_projects_per_projtype <- num_unique_bidders_per_projtype %>% left_join(num_projects_per_projtype)
  
potential_bidders_by_contract <- bidder_project_level_descriptions %>%
  select(contract_no, district_grouped, project_type_id, bid_open_year_grouped, proj_manager_id, engineer_id, designer_id) %>%
  left_join(num_unique_bidders_per_projtype, by = c("district_grouped", "project_type_id", "bid_open_year_grouped")) %>%
  unique()


potential_bidders_by_contract <- potential_bidders_by_contract %>%
  left_join(num_bidders_seen_per_proj, by = "contract_no") %>%
  group_by(potential_bidder_bin_id) %>%
  mutate(
    max_num_bidders__seen = max(num_bidders_seen)
  ) %>%
  ungroup()
  
potential_bidders_by_contract <- potential_bidders_by_contract %>%
  relocate(contract_no, potential_bidder_bin_id, max_num_bidders__unique, max_num_bidders__seen, num_bidders_seen)

## Add some summary stats from observed bids (and estimated alphas)
gmm_output_summary <- gmm_output %>%
  group_by(contract_no) %>%
  summarize(
    min_alpha = min(alpha),
    max_alpha = max(alpha)
  )

potential_bidders_by_contract <- potential_bidders_by_contract %>%
  left_join(gmm_output_summary, by = "contract_no") 

write_csv(potential_bidders_by_contract, paste0(cf_inputs_dir,"potential_bidders_by_contract.csv"))


### prep for alpha model ## 
alpha_model_df <- alpha_model_df %>%
  left_join(potential_bidders_by_contract %>% select(contract_no, potential_bidder_bin_id, project_type_id))

write_csv(alpha_model_df, paste0(cf_inputs_dir,"alphas_by_auction_with_features.csv"))

