library(dplyr)

summary_info_in_out <- function(df){
  
  young_in_id <- df %>% 
    filter(age_upon_intake_.years. == min(age_upon_intake_.years.)) %>% 
    pull(animal_id_intake) %>% 
    unique()
  old_in_id <- df %>% 
    filter(age_upon_intake_.years. == max(age_upon_intake_.years.)) %>% 
    pull(animal_id_intake) %>% 
    unique()
  
  young_out_id <- df %>% 
    filter(age_upon_intake_.years. == min(age_upon_outcome_.years.)) %>% 
    pull(animal_id_outcome) %>% 
    unique()
  old_out_id <- df %>% 
    filter(age_upon_intake_.years. == max(age_upon_outcome_.years.)) %>% 
    pull(animal_id_outcome) %>% 
    unique()
  
  #forming the return list
  return_list <- list(
    
    # Average time that an animal spends in the AAC
    avg_time_in_shelter_days = mean(df$time_in_shelter_days),
    
    # Average age in years of animals coming into the AAC
    avg_age_in = mean(df$age_upon_intake_.years.),
    
    # Average age in years of animals leaving the AAC
    avg_age_out = mean(df$age_upon_outcome_.years.),
    
    # The animal type with the longest average time in AAC
    longest_avg_time_animal = df %>% 
      group_by(animal_type) %>% 
      mutate(time = mean(time_in_shelter_days)) %>% 
      ungroup() %>% 
      filter(time == max(time, na.rm = T)) %>% 
      pull(animal_type) %>% 
      unique(),
    
    longest_avg_time = df %>% 
      group_by(animal_type) %>% 
      mutate(time = mean(time_in_shelter_days)) %>% 
      ungroup() %>% 
      filter(time == max(time, na.rm = T)) %>% 
      pull(time) %>% 
      unique(),
    
    # The animal type with the shortest average time in AAC
    shortest_avg_time_animal = df %>% 
      group_by(animal_type) %>% 
      mutate(time = mean(time_in_shelter_days)) %>% 
      ungroup() %>% 
      filter(time == min(time, na.rm = T)) %>% 
      pull(animal_type) %>% 
      unique(),
    
    shortest_avg_time = df %>% 
      group_by(animal_type) %>% 
      mutate(time = mean(time_in_shelter_days)) %>% 
      ungroup() %>% 
      filter(time == min(time, na.rm = T)) %>% 
      pull(time) %>% 
      unique(),
    
    in_out_count = df %>% 
      nrow()
    
  )
}

summary_info_in <- function(df){
  
  # ID of the animal who has been taken into the AAC most
  # frequently
  freq_case_id = df %>% 
    group_by(animal_id) %>% 
    mutate(count = length(animal_id)) %>% 
    ungroup %>% 
    filter(count == max(count, na.rm = T)) %>% 
    select(animal_id, animal_type, breed, count) %>% 
    unique() %>% 
    pull(animal_id)
  
  #Forming the return list
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
      nrow(),
    
    # The name of the animal who has been taken into the AAC most
    # frequently
    freq_case_name = df %>% 
      filter(animal_id == freq_case_id) %>% 
      pull(name) %>% 
      unique(),
    
    # The animal_type of the animal who has been taken into the AAC
    # most frequently
    freq_case_animal = df %>% 
      filter(animal_id == freq_case_id) %>% 
      pull(animal_type) %>% 
      unique(),
    
    # The breed of the animal who has been taken into the AAC
    # most frequently
    freq_case_breed = df %>% 
      filter(animal_id == freq_case_id) %>% 
      pull(breed) %>% 
      unique(),
    
    # The sex of the animal who has been taken into the AAC
    # most frequently
    freq_case_sex = df %>% 
      filter(animal_id == freq_case_id) %>% 
      pull(sex_upon_intake) %>% 
      unique(),
    
    # The number of times the animal who has been taken into the AAC
    # most frequently has been taken into the AAC
    freq_case_count = df %>% 
      filter(animal_id == freq_case_id) %>% 
      mutate(count = length(animal_id)) %>% 
      pull(count) %>% 
      unique()
    
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
    out_count = df %>% 
      nrow(),
      
    # The animal type that is most likely
    # to be adopted
    most_adopted_animal = out_prop_df %>% 
      filter(outcome_type == "Adoption") %>% 
      filter(out_percent == max(out_percent, na.rm = T)) %>% 
      pull(animal_type),
    
    # The percent likelihood of the animal type that is
    # most likely to be adopted is to be adopted
    most_adopted_perc = out_prop_df %>% 
      filter(outcome_type == "Adoption") %>% 
      filter(out_percent == max(out_percent, na.rm = T)) %>% 
      pull(out_percent),
    
    # The percent likelihood of the animal type that is
    # most likely to be euthanized is to be euthanized
    most_euthanized_animal = out_prop_df %>% 
      filter(outcome_type == "Euthanasia") %>% 
      filter(out_percent == max(out_percent)) %>% 
      pull(animal_type),
    
    # The greatet percentage of an animal tupe
    most_euthanized_perc = out_prop_df %>% 
      filter(outcome_type == "Euthanasia") %>% 
      filter(out_percent == max(out_percent)) %>% 
      pull(out_percent)
  )
}

