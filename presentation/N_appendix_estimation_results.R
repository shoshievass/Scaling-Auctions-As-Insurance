##==================================================
##
## Script name: Tables and Figures N. Appendix Estimation Results
##
## Project: Scaling Auctions as Insurance 
##
## Purpose of script:  Create data description tables and figures
##
## Input:  
##
##
## Output: outputs/estimation_results/app_table8.tex
##         outputs/estimation_results/app_table9.tex
##         outputs/estimation_results/app_table10.tex
##         outputs/estimation_results/app_fig6.jpg
##         outputs/estimation_results/app_fig7.jpg
##         outputs/estimation_results/app_fig8a.jpg
##         outputs/estimation_results/app_fig8b.jpg

## Packages ##
library(tidyverse)
library(latex2exp)
library(ggthemes)
library(kableExtra)
library(lfe)
library(stringr)
library(glue)

# Create .library if it doesn't exist
if (!dir.exists("presentation/.library")) {
  dir.create("presentation/.library")
}

# Check if binscattr is installed
if (!requireNamespace("binscattr", lib.loc="presentation/.library/")) {
  # If not, install it
  devtools::install_local(
    "presentation/binscattr/binscattr/", 
    upgrade="ask", 
    lib="presentation/.library/",
    build_vignettes = FALSE
  )
}

# Load binscattr
library(binscattr, lib.loc="presentation/.library/")

## Load the data
### 1st stage estimates + transformed input data
load(file.path("data","estimation_step1_output_minfit.rdata"))

### 2nd stage estimates
gmm_output <- read_csv(file.path("data","estimation_step3","second_stage_estimates_summary.csv"))

## Load 2nd stage estimates ##
gmm_est_suffix = "_summary.csv"
gmm_bid_suffix = "_bid_fit.csv"

gmm_output <- read_csv(file.path("data","estimation_step3",paste0("second_stage_estimates", gmm_est_suffix)))
gmm_bid_fits <- read_csv(file.path("data","estimation_step3",paste0("second_stage_estimates", gmm_bid_suffix)))

## directory names for outputs
output_dir = file.path("outputs","estimation_results")
dir.create(output_dir)

theme_set(theme_minimal())

saveLastFig <- function(fname){
  fpath = file.path(output_dir, paste0(fname, ".jpg"))
  ggsave(fpath, height=7, width=7 * 4/3)
}

## To skip annoying warnings if just running through to replicate
options(warn=-1)



## Table Column Names ##
summary_col_names <- c('Project Type', 'Mean', 'SD', '25%', '50%', '75%')
summary_col_names_numbids <- c('Number of Bidders', 'Mean', 'SD', '25%', '50%', '75%')

## Appendix: Table ?? ## 
#Get from "parameter_estimates_table.r"
##


## Appendix Table 8: Second Stage Bootstrap Errors and Quantiles ##
## Note: The table in the paper has the rows shuffled from the table here ##

bootstrap_sumstats <- read_csv(file.path("data","estimation_step3", "two_stage_bootstrap_errors_paramquantilesum.csv")) %>%
  filter(!str_detect(parameter, "trunc")) 


param_names =  bootstrap_sumstats %>% select(parameter) %>%
  mutate(
    param_full = parameter
  ) %>%
  separate(parameter,into = c("param_name", "param_stat"), sep = "_") %>%
  mutate(
    param_stat = ifelse(str_detect(param_stat, "median"), "50", param_stat)
  ) %>%
  mutate(
    param_name = case_when(
      param_name == "alpha" ~ "alpha",
      param_name == "gamma" ~ "gamma",
      param_name == "mkp" ~ "Markup"
    ),
    param_stat = ifelse(str_detect(param_stat,"[0-9]"), paste0(str_extract(param_stat, "[0-9]+"), "%"), str_to_title(param_stat))
  ) %>%
  mutate(
    pretty_param = glue("{param_stat} {param_name}")
  ) %>%
  transmute(
    parameter = param_full,
    pretty_param 
  )

bootstrap_col_names <- c('Parameter', 'Estimate', 'SD', 'SD within CI', '2.5%', '97.5%')

sink(file.path(output_dir,"app_table8.tex"), append=FALSE, split=FALSE)
bootstrap_sumstats %>%
  left_join(param_names) %>%
  relocate(pretty_param) %>%
  select(-parameter, -mean, -median) %>%
  data.frame() %>%
  magrittr::set_colnames(
    str_to_title(colnames(.))
  ) %>%
  rename_with(
    ~bootstrap_col_names, .cols = everything()
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate_if(is.numeric, format,big.mark=",",scientific=FALSE) %>%
  kable(format = "latex",
        booktabs = T,
        align = "c",
        caption = "Second Stage Bootstrap Errors and Quantiles") %>%
  kable_styling(latex_options =c("scale_down"))
sink()


## Appendix Fig 6 ##
demo_project_item_df$qa_model_fit = qa_model_fit
demo_project_item_df$sigma_t = sigma_t_fit
binscatter(data = demo_project_item_df, y = q_at, x = qa_model_fit,pos="") +
  labs(
    y = "Actual Quantity",
    x = "Predicted Quantity Mean"
  ) + theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  ) + theme(aspect.ratio = 3/4)

saveLastFig("app_fig6")
## End of Fig 6 ##

## Appendix Table 9 ## 
sink(file.path(output_dir,"app_table9.tex"), append=FALSE, split=FALSE)

qa_fit_lm <- (lm(q_at ~ qa_model_fit, data = demo_project_item_df))
stargazer::stargazer(qa_fit_lm,
                     dep.var.labels = "Actual Quantity",
                     covariate.labels = "Predicted Quantity",
                     title = "Regression report for \\Cref{fig:qabinscatter}",
                     label = "table:reg:qabinscatter",
                     omit.stat=c("adj.rsq","f", "ser"))

sink()
## End of Table 9 ##


## Appendix: Fig 8a ##
w = 1000

minimal_match_df <- empirical_bid_data %>%
  filter(bridge==1) %>%
  select(
    contract_no,
    bidder_id_sequential,
    bid_unit_price,
    office_unit_price,
    item_id_sequential
  ) 


gmm_bid_fits_matched <-
  gmm_bid_fits %>%
  left_join(
    minimal_match_df, by = c("contract_no" = "contract_no", "bidder_id" = "bidder_id_sequential", "item_id" = "item_id_sequential")
  ) %>%
  mutate(
    bid_fit = bid_fit*w,
    bid_fit_w_error = bid_fit_w_error*w,
    nu = nu*w
  ) %>%
  mutate(
    abs_bid_erorr = abs(nu)
  )


p <- seq(0.0001,0.9999,0.0001)
bid_fit <- gmm_bid_fits_matched$bid_fit_w_error/w
bid_data <- gmm_bid_fits_matched$bid_unit_price/w
ggplot() +
  geom_point(aes(x = quantile(bid_fit, p), y = quantile(bid_data, p)), color="#0072B2") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Predicted Bid Quantiles",
    y = "Data Bid Quantiles"
  ) +
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
  theme_minimal() + theme(aspect.ratio = 3/4)

saveLastFig("app_fig8a")
## End of Fig 8a ##

## Table 10 ##
sink(file.path(output_dir,"app_table10.tex"), append=FALSE, split=FALSE)

bid_fit_lm <- (lm(bid_unit_price ~ bid_fit_w_error, data = gmm_bid_fits_matched))
stargazer::stargazer(bid_fit_lm, dep.var.labels = "Data Bid", covariate.labels = "Predicted Bid", omit.stat=c("adj.rsq","f", "ser"))

sink()
## End of Table 10 ##

## Figure 7 ##
gmm_bid_fits_matched %>%
  mutate(
    bid_unit_price = bid_unit_price/w,
    bid_fit_w_error = bid_fit_w_error/w
  ) %>%
  ggplot(aes(y = bid_unit_price, x = bid_fit_w_error)) +
  geom_point()+
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    x = "Predicted Bid",
    y = "Data Bid"
  ) +
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  ) + theme(aspect.ratio = 3/4)


saveLastFig("app_fig7")
## End of Figure 7 ##

## Figure 8b ##
## We compute this check in tandem with the moral hazard exercise ##
## But plot just the `baseline` (non-moral-hazard) results, of course ##

input_dir = file.path("data", "estimation_step8_moralhazard")
mh_results <-  read_csv(file.path(input_dir, "mh_results_nocap.csv"))

model_preds <- mh_results %>%
  filter(tag=="baseline-estimated")%>%
  select(
    contract,
    winning_score
  ) %>%
  rename(contract_no=contract)

mini_df <- empirical_bid_data %>% select(project_bidder_id, contract_no, bridge, rank) %>% group_by(project_bidder_id) %>% summarize_all(first)

bidder_project_level_df <- bidder_project_level_df %>%
  left_join(mini_df)

cost_df <- bidder_project_level_df %>%
  dplyr::filter(bridge==1) %>%
  select(
    contract_no,
    bidder_id_seq,
    project_bidder_id,
    bidder_score,
    rank
  ) %>%
  left_join(gmm_output, by= c("contract_no" = "contract_no", "bidder_id_seq" = "bidder_id")) %>%
  filter(complete.cases(.))

winner_scores <- cost_df %>%
  select(
    contract_no,
    rank,
    bidder_score
  ) %>%
  filter(rank==1)

compare <- model_preds %>%
  left_join(winner_scores)

compare %>%
  mutate(
    bidder_score = bidder_score/(1e6),
    winning_score = winning_score/(1e3)
  ) %>%
  ggplot(aes(x=bidder_score, y = winning_score)) + geom_point() +
  scale_x_continuous(labels = scales::dollar, lim = c(0,17))+
  scale_y_continuous(labels = scales::dollar, lim = c(0,17))+
  labs(
    x = "Data Winning Score ($M)",
    y = "Predicted Winning Score ($M)"
  ) +
  geom_abline(aes(intercept=0, slope = 1),color="red",linetype="dashed") +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
  ) + theme(aspect.ratio = 3/4)

saveLastFig("app_fig8b")
## End of Figure 8b ##
