library(shiny)
source("scripts/summary_info.R")


in_df <- read.csv(
  file = "data/aac_intakes.csv",
  encoding = "UTF-8",
  stringsAsFactors = F)

out_df <- read.csv(
  file = "data/aac_outcomes.csv",
  encoding = "UTF-8",
  stringsAsFactors = F)

in_out_df <- read.csv(
  file = "data/aac_intakes_outcomes.csv",
  encoding = "UTF-8",
  stringsAsFactors = F)



