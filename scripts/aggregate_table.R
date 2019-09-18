library(dplyr)

ag_table <- function(df){
  ret <- df %>%
    group_by(animal_type) %>%
    mutate(
      `Frequency` = length(animal_type),
      `Avg Intake Age` = round(mean(age_upon_intake_.years.),2),
      `Avg Outcome Age` = round(mean(age_upon_outcome_.years.),2)
    ) %>%
    ungroup() %>% 
    group_by(animal_type, outcome_type) %>% 
    mutate(
      `Adoptions` = length(outcome_type)
    ) %>% 
    filter(outcome_type == "Adoption") %>% 
    group_by(animal_type) %>% 
    mutate(
      `Adoption %` = `Adoptions` / `Frequency`
        ) %>% 
    select(
      animal_type,
      `Frequency`,
      `Avg Intake Age`,
      `Avg Outcome Age`,
      `Adoption %`
           ) %>% 
    unique()
  ret
}

