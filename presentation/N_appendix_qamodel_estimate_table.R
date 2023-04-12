library(tidyverse)
library(skimr)
library(ggridges)
# library(binscattr); library(latex2exp)
library(ggthemes)

# library(huxtable)
# library(broom)

library("shinystan")

options(browser = "chrome")

gmm_input_1st_stage_data <- "estimation_step1_output_fullfit.rdata"
load(file.path("data", gmm_input_1st_stage_data))

## Table 7 ##

## The ShinyStan application has a method to export a latex table of the estimates
## This is how we got Table 7 in the paper (after relabeling parameters for easy reading and pretty printing)
## We couldn't figure out how to automate this step
launch_shinystan(qa_fit)

## End of Table 7 ##
