library(ggplot2)
library(ggradar)
library(dplyr)
library(plotly)

in_df <- read.csv("../data/aac_intakes.csv", stringsAsFactors = F)
out_df <- read.csv("../data/aac_outcomes.csv", stringsAsFactors = F)
in_out_df <- read.csv("../data/aac_intakes_outcomes.csv", stringsAsFactors = F)
################################################################################
# Function to convert ggplots to plotly in a standard format for the project
ggplotly2 <- function(plot){
  ggplotly(plot, tooltip = "text") %>%
    style(hoverlabel = list(bgcolor = "white"), hoveron = "text")
}

#G1#############################################################################
# Scatterplot of age upon intake (in days) and total time spent in 
# shelter (in days)
scatter <- function(in_out_df, animal, lower_year, upper_year, outcome){
  df <- in_out_df %>% 
    filter(animal_type %in% animal, intake_year >= lower_year, intake_year <= upper_year, outcome_type == outcome)
  
  ggplot(df, aes(x = age_upon_intake_.years., y = time_in_shelter_days, color = animal_type))+
    geom_point(aes(text=sprintf("Animal: %s
                                Breed: %s
                                Sex: %s
                                Age: %s Year(s)
                                Time in Shelter: %s days",
                                animal_type,
                                breed,
                                sex_upon_intake,
                                round(age_upon_intake_.years., 2),
                                round(time_in_shelter_days, 2))), alpha = 0.4)+
    xlab("Age (in years)")+
    ylab("Time in Shelter (in days)")+
    ggtitle(paste("Age of", toString(animal), "at Intake vs Total Continuous Time in Shelter"))+
    theme_bw()
}

#G2#############################################################################
# Time series plot of animal adoptions by animal type
time_series <- function(in_out_df, animal, sep){
  df <- in_out_df %>% 
    filter(outcome_type == "Adoption", animal_type %in% animal) %>% 
    mutate(date = substr(outcome_datetime, 1,7)) %>% 
    group_by(date) %>% 
    mutate(total_freq = length(date)) %>% 
    group_by(.dots=c("date", "animal_type")) %>% 
    mutate(sep_freq = length(animal_type)) %>% 
    ungroup() %>% 
    filter(date != min(date), date != max(date)) %>% 
    select(
      date,
      animal_type,
      age_upon_outcome_.days.,
      sex_upon_outcome,
      total_freq,
      sep_freq)
  
  if(sep){
    ggplot(df, aes(x = date, y = sep_freq, color = animal_type, group = animal_type))+
      geom_point(aes(text=sprintf("Date: %s<br>Animal: %s<br>Adoptions: %s", date, animal_type, sep_freq))) + geom_line(aes(text = ""))+
      scale_x_discrete("Date",
                       breaks = c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"),
                       labels = c("2014-01" = "2014",
                                  "2015-01" = "2015",
                                  "2016-01" = "2016",
                                  "2017-01" = "2017",
                                  "2018-01" = "2018"))+
      ylab("Adoptions")+
      ggtitle(paste(toString(animal), "Adoption Trends between 2014 and 2018"))+
      theme_bw()
    
  } else {
    ggplot(df, aes(x = date, y = total_freq, group = animal_type))+
      geom_point(aes(text=sprintf("Date: %s<br>Total Adoptions: %s", date, total_freq))) + geom_line( aes(text = ""))+
      scale_x_discrete("Date",
                       breaks = c("2014-01", "2015-01", "2016-01", "2017-01", "2018-01"),
                       labels = c("2014-01" = "2014",
                                  "2015-01" = "2015",
                                  "2016-01" = "2016",
                                  "2017-01" = "2017",
                                  "2018-01" = "2018"))+
      ylab("Total Adoptions")+
      ggtitle(paste("Total", toString(animal), "Adoption Trends between 2014 and 2018"))+
      theme_bw()
  }
}

#G3#############################################################################
# Radar chart displaying the outcome rates of all breeds of a certain
# animal type (1 radio 2selectors)

radar <- function(in_out_df, breed1, breed2, animal){

}