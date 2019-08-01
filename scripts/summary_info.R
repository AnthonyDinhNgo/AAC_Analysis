library(dplyr)

in_df <- read.csv("../data/aac_intakes.csv", stringsAsFactors = F)
out_df <- read.csv("../data/aac_outcomes.csv", stringsAsFactors = F)
in_out_df <- read.csv("../data/aac_intakes_outcomes.csv", stringsAsFactors = F)

summary_info_in_out <- function(df){
  return_list <- list(
    
    # Average time that an animal spends in the AAC
    avg_time_in_shelter_days <- mean(df$time_in_shelter_days),
    
    # Average age in years of animals coming into the AAC
    avg_age_in = mean(df$age_upon_intake_.years),
    
    # Average age in years of animals leaving the AAC
    avg_age_out = mean(df$age_upon_outcome_.years)
    
  )
}

summary_info_in <- function(df){
  return_list <- list(
    
    # The animal type that is most often taken in by
    # the AAC
    pop_animal_in = df %>% 
      group_by(animal_type) %>% 
      mutate(observations = length(animal_type)) %>% 
      select(animal_type, observations) %>% 
      unique() %>% 
      ungroup() %>% 
      filter(observations == max(observations)) %>% 
      pull(animal_type),
    
    # The number of animals that came into the AAc in the dataset
    in_count = df %>% 
      nrows()
  )
}

# Function for calculatring the summary information
# for the outcome dataframe
summary_info_out <- function(df){
  
  # Aggregated dataframe with the proportions of each
  # outcome type for each animal type
  out_prop_df <-  df %>% 
    group_by(animal_type, outcome_type) %>% 
    mutate(out_count = length(animal_type)) %>% 
    group_by(animal_type) %>% 
    mutate(
      count = length(animal_type),
      out_prop = out_count / length(animal_type)) %>% 
    mutate(out_percent = round(out_prop, 3) * 100) %>% 
    select(
      animal_type,
      outcome_type,
      count,
      out_count,
      out_prop,
      out_percent
      ) %>% 
    ungroup() %>% 
    unique()
  
  # Forming the return list
  return_list <- list(
    
    # Number of outcomes there were in the dataset
    out_count <- df %>% 
      nrow(),
      
    # The animal type that is most likely
    # to be adopted
    most_adopted_animal = out_prop_df %>% 
      filter(outcome_type == "Adoption") %>% 
      filter(out_percent == max(out_percent, na.rm = T)) %>% 
      pull(animal_type),
    
    # The animal type that is most likely to be
    # euthanized
    most_euthanized_animal = out_prop_df %>% 
      filter(outcome_type == "Euthanasia") %>% 
      filter(out_percent == max(out_percent)) %>% 
      pull(animal_type)
  )
}

out_info <- summary_info_out(out_df)
in_info <- summary_info_in(in_df)
in_out_info <- summary_info_in_out(in_out_df)

