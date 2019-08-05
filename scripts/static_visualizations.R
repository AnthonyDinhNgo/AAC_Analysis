library(ggplot2)
library(dplyr)

in_df <- read.csv("../data/aac_intakes.csv", stringsAsFactors = F)
out_df <- read.csv("../data/aac_outcomes.csv", stringsAsFactors = F)
in_out_df <- read.csv("../data/aac_intakes_outcomes.csv", stringsAsFactors = F)

age_and_time_in_AAC<- function(df) {
  ggplot(df, aes(age_upon_intake_.days., time_in_shelter_days))+
    geom_jitter(color = animal_type)
}

age_time_AAC <- age_and_time_in_AAC(in_out_df)
age_time_AAC
