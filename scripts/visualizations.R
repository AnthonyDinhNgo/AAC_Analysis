library(ggplot2)
library(dplyr)
library(plotly)

################################################################################
# Function to convert ggplots to plotly in a standard format for the project
ggplotly2 <- function(plot){
  ggplotly(plot, tooltip = "text") %>%
    style(hoverlabel = list(bgcolor = 'rgba(0,0,0,0)'), hoveron = "text") %>% 
    layout(paper_bgcolor = 'rgba(0,0,0,0)')%>% 
    layout(plot_bgcolor='rgba(0,0,0,0)')
}

#G1#############################################################################
# Scatterplot of age upon intake (in days) and total time spent in 
# shelter (in days)
scatter <- function(in_out_df, animal, lower_year, upper_year, outcome){
  df <- in_out_df %>% 
    filter(
      animal_type %in% animal,
      intake_year >= lower_year,
      intake_year <= upper_year,
      outcome_type == outcome)
  
  ggplot(df, 
         aes(
           x = age_upon_intake_.years.,
           y = time_in_shelter_days,
           color = animal_type))+
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
    ggtitle(paste("Age of",
                  toString(animal),
                  "at Intake vs Days in Shelter"))+
    theme_bw()+
    scale_color_discrete(name = "Animal")
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
    ggplot(df, 
           aes(
             x = date,
             y = sep_freq,
             color = animal_type,
             group = animal_type))+
      geom_point(
        aes(
          text=sprintf(
            "Date: %s<br>Animal: %s<br>Adoptions: %s",
            date, animal_type, sep_freq)))+
      geom_line(aes(text = ""))+
      scale_x_discrete("Date",
                       breaks = c("2014-01",
                                  "2015-01",
                                  "2016-01",
                                  "2017-01",
                                  "2018-01"),
                       labels = c("2014-01" = "2014",
                                  "2015-01" = "2015",
                                  "2016-01" = "2016",
                                  "2017-01" = "2017",
                                  "2018-01" = "2018"))+
      ylab("Adoptions")+
      ggtitle(paste(toString(animal),
                    "Adoption Trends between 2014 and 2018"))+
      theme_bw()+
      scale_color_discrete(name = "Animal")
    
  } else {
    ggplot(df, aes(x = date, y = total_freq, group = animal_type))+
      geom_point(
        aes(
          text=sprintf("Date: %s<br>Total Adoptions: %s",
                       date, total_freq))) +
      geom_line( aes(text = ""))+
      scale_x_discrete("Date",
                       breaks = c("2014-01",
                                  "2015-01",
                                  "2016-01",
                                  "2017-01",
                                  "2018-01"),
                       labels = c("2014-01" = "2014",
                                  "2015-01" = "2015",
                                  "2016-01" = "2016",
                                  "2017-01" = "2017",
                                  "2018-01" = "2018"))+
      ylab("Total Adoptions")+
      ggtitle(paste("Total",
                    toString(animal),
                    "Adoption Trends between 2014 and 2018"))+
      theme_bw()
  }
}

#G3#############################################################################
# Radar chart displaying the outcome rates of all breeds of a certain
# animal type (1 radio 2selectors)

radar <- function(in_out_df, breed1_name, breed2_name, animal, outcome){
  breed1 <- breed1_name
  if(breed1 == "All"){
    breed1 <- in_out_df %>% 
      filter(animal_type == animal) %>% 
      select(breed) %>% 
      unique() %>% 
      pull()
  }
  
  breed2 <- breed2_name
  if(breed2 == "All"){
    breed2 <- in_out_df %>% 
      filter(animal_type == animal) %>%
      select(breed) %>% 
      unique() %>% 
      pull()
  }
  
  title_var <- paste(animal, "Outcome Proportions")
  thet <-  c(
    "Adoption",
    "Return to Owner",
    "Euthanasia",
    "Died",
    "Transfer"
  )
  df1 <- NULL
  df2 <- NULL

  if(outcome){
    df1 <- in_out_df %>% 
      filter(animal_type == animal, breed %in% breed1) %>%
      mutate(
             adoption_prop = (sum(outcome_type == "Adoption")
                              /length(animal_type)),
             return_to_owner_prop = (sum(outcome_type == "Return to Owner")
                                     /length(animal_type)),
             euthanasia_prop = (sum(outcome_type == "Euthanasia")
                                /length(animal_type)),
             died_prop = (sum(outcome_type == "Died")
                          /length(animal_type)),
             transfer_prop = (sum(outcome_type == "Transfer")
                              /length(animal_type))
      ) %>% 
      select(
        adoption_prop,
        return_to_owner_prop,
        euthanasia_prop,
        died_prop,
        transfer_prop
             ) %>% 
      unique()
    
    df2 <- in_out_df %>% 
      filter(animal_type == animal, breed %in% breed2) %>%
      mutate(
        adoption_prop = (sum(outcome_type == "Adoption")
                         /length(animal_type)),
        return_to_owner_prop = (sum(outcome_type == "Return to Owner")
                                /length(animal_type)),
        euthanasia_prop = (sum(outcome_type == "Euthanasia")
                           /length(animal_type)),
        died_prop = (sum(outcome_type == "Died")
                     /length(animal_type)),
        transfer_prop = (sum(outcome_type == "Transfer")
                         /length(animal_type))
      ) %>% 
      select(
        adoption_prop,
        return_to_owner_prop,
        euthanasia_prop,
        died_prop,
        transfer_prop
             ) %>% 
      unique()
  } else {
    thet <- c(
      "Stray",
      "Public Assist",
      "Owner Surrender",
      "Euthanasia Request",
      "Wildlife"
    )
    
    title_var <- paste(animal, "Intake Proportions")
    
    df1 <- in_out_df %>% 
      filter(animal_type == animal, breed %in% breed1) %>%
      mutate(
        stray_prop = (sum(intake_type == "Stray")
                      /length(animal_type)),
        public_assist_prop = (sum(intake_type == "Public Assist")
                              /length(animal_type)),
        owner_surr_prop = (sum(intake_type == "Owner Surrender")
                           /length(animal_type)),
        euth_req_prop = (sum(intake_type == "Euthanasia Request")
                         /length(animal_type)),
        wild_prop = (sum(intake_type == "Wildlife")
                     /length(animal_type))
      ) %>% 
      select(
        stray_prop,
        public_assist_prop,
        owner_surr_prop,
        euth_req_prop,
        wild_prop
      ) %>% 
      unique()
    
    df2 <- in_out_df %>% 
      filter(animal_type == animal, breed %in% breed2) %>%
      mutate(
        stray_prop = (sum(intake_type == "Stray")
                      /length(animal_type)),
        public_assist_prop = (sum(intake_type == "Public Assist")
                              /length(animal_type)),
        owner_surr_prop = (sum(intake_type == "Owner Surrender")
                           /length(animal_type)),
        euth_req_prop = (sum(intake_type == "Euthanasia REquest")
                         /length(animal_type)),
        wild_prop = (sum(intake_type == "Wildlife")
                     /length(animal_type))
      ) %>% 
      select(
        stray_prop,
        public_assist_prop,
        owner_surr_prop,
        euth_req_prop,
        wild_prop
      ) %>% 
      unique()
  }
  
  p <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) %>%
    add_trace(
      r = as.numeric(as.vector(df1[1,])),
      theta = thet,
      name = breed1_name
    )%>%
    add_trace(
      r = as.numeric(as.vector(df2[1,])),
      theta = thet,
      name = breed2_name
    )%>%
    layout(
      title = title_var,
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
        )
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
  p
}


#One-Off Vis####################################################################
#Black Cats#####################################################################
black_cats_in_out_time <- function(in_df, out_df){
  intakes <- in_df %>% 
    filter(animal_type == "Cat",
           intake_type == "Owner Surrender",
           color == "Black") %>% 
    mutate(
      year_month = substr(datetime, 0,7)
    ) %>% 
    group_by(year_month) %>% 
    mutate(
      in_freq = length(year_month)
    ) %>% 
    select(year_month, in_freq) %>% 
    unique() %>% 
    arrange(year_month)
  
  outcomes <- out_df %>% 
    filter(animal_type == "Cat",
           outcome_type == "Adoption",
           color == "Black"
    ) %>% 
    mutate(
      year_month = substr(datetime, 0,7)
    ) %>% 
    group_by(year_month) %>% 
    mutate(
      out_freq = length(year_month)
    ) %>% 
    select(year_month, out_freq) %>% 
    unique() %>% 
    arrange(year_month)
  
  df <- merge(intakes, outcomes)
  
  p <- plot_ly(df) %>% 
    add_trace(
      x = ~year_month,
      y = ~in_freq, 
      type = 'scatter',
      mode = 'lines',
      name = 'Owner Surrenders',
      hoverinfo = "text",
      text = ~paste("Owner Surrender\nDate : ", year_month,
                    "\nFrequency : ", in_freq)
    ) %>% 
    add_trace(
      x = ~year_month,
      y = ~out_freq,
      type = 'scatter',
      mode = 'lines',
      name = 'Adoptions',
      hoverinfo = "text",
      text = ~paste("Adoption\nDate : ", year_month,
                    "\nFrequency : ", out_freq)
    )%>%
    layout(
      title = "Adoptions vs Owner Surrenders of Black Cats between 2014 and 2018",
      xaxis = list(title = 'Date'),
      yaxis = list(side = 'left', title = 'Frequency'),
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
        )
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
  p
}
#nonBlack Cats##################################################################
nonblack_cats_in_out_time <- function(in_df, out_df){
  intakes <- in_df %>% 
    filter(animal_type == "Cat",
           intake_type == "Owner Surrender",
           color != "Black") %>% 
    mutate(
      year_month = substr(datetime, 0,7)
    ) %>% 
    group_by(year_month) %>% 
    mutate(
      in_freq = length(year_month)
    ) %>% 
    select(year_month, in_freq) %>% 
    unique() %>% 
    arrange(year_month)
  
  outcomes <- out_df %>% 
    filter(animal_type == "Cat",
           outcome_type == "Adoption",
           color != "Black"
    ) %>% 
    mutate(
      year_month = substr(datetime, 0,7)
    ) %>% 
    group_by(year_month) %>% 
    mutate(
      out_freq = length(year_month)
    ) %>% 
    select(year_month, out_freq) %>% 
    unique() %>% 
    arrange(year_month)
  
  df <- merge(intakes, outcomes)
  
  p <- plot_ly(df) %>% 
    add_trace(
      x = ~year_month,
      y = ~in_freq, 
      type = 'scatter',
      mode = 'lines',
      name = 'Owner Surrenders',
      hoverinfo = "text",
      text = ~paste("Owner Surrender\nDate : ", year_month,
                    "\nFrequency : ", in_freq)
    ) %>% 
    add_trace(
      x = ~year_month,
      y = ~out_freq,
      type = 'scatter',
      mode = 'lines',
      name = 'Adoptions',
      hoverinfo = "text",
      text = ~paste("Adoption\nDate : ", year_month,
                    "\nFrequency : ", out_freq)
    )%>%
    layout(
      title = "Adoptions vs Owner Surrenders of non-Black Cats between 2014 and 2018",
      xaxis = list(title = 'Date'),
      yaxis = list(side = 'left', title = 'Frequency'),
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
        )
      ),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
  p
}


#Pitbull vs All Outcomes########################################################
bars <- function(out_df){
  df <- out_df %>% 
    mutate(
      isPitbull = (breed %in% c("Pit Bull", "Pit Bull Mix"))
    ) %>% 
    group_by(isPitbull) %>% 
    mutate(freq = length(isPitbull)) %>% 
    group_by(isPitbull, outcome_type) %>% 
    mutate(outcome_prop = length(outcome_type) / freq)%>% 
    select(
      isPitbull,
      outcome_type,
      outcome_prop
    ) %>% 
    filter(outcome_type != "") %>% 
    unique()
  
  ggplot(df)+
    geom_bar(aes(x = outcome_type, y = outcome_prop, fill = isPitbull), 
    stat="identity", position = "dodge", width = 0.7)+
    scale_fill_manual("Result\n",
                      values = c("red","blue"), 
                      labels = c("Other", "Pit Bull or Pit Bull Mix")) +
    labs(
      title = "Pitbull Outcomes vs Other",
      x="\nOutcome",
      y="Proportion\n") +
    theme(
      rect = element_rect(fill = "transparent"),
      axis.text.x = element_text(angle=-60, hjust=1)
    )
}

bars_df <- bars(out_df)
bars_df
#Pitbull vs All Intakes#########################################################