library(shiny)

intro_page <- tabPanel(
  "Intro",
  titlePanel("Intro page"),
  em("[Intro Placeholder]")
  )
################################################################################
scatter_tab <- tabPanel(
  "Scatterplot",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      checkboxGroupInput("animal_list", label = h3("Animal Type"), 
                         choices = list("Dog" = "Dog", "Cat" = "Cat", "Bird" = "Bird", "Other" = "Other"),
                         selected = "Dog"),
      sliderInput("years", label = h3("Years"), min = 2013, 
                  max = 2018, value = c(2015, 2016)),
      selectInput("out", label = h3("Outcome"), 
                  choices = list("Adoption" = "Adoption",
                                 "Return to Owner" = "Return to Owner",
                                 "Transfer" = "Transfer",
                                 "Euthanasia" = "Euthanasia",
                                 "Died"), 
                  selected = "Adoption")
      
    ),
    mainPanel = mainPanel(
      div(
        class = "info-content",
        p("With this table, we want to be able to understand how age affects
            the value of a player. Below, we can see the distribution of the
            transfer fees of the different ages of players"),
        hr(),
        plotlyOutput("p1")
      )
    )
  )
)

################################################################################
time_tab <- tabPanel(
  "Time Series",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      checkboxInput("sep", label = "Separate Lines", value = T),
      checkboxGroupInput("animal_list_time", label = h3("Animal Type"), 
                         choices = list("Dog" = "Dog", "Cat" = "Cat", "Bird" = "Bird", "Other" = "Other"),
                         selected = "Dog")
    )
  ,
  mainPanel = mainPanel(
    div(
      class = "info-content",
      p("With this table, we want to be able to understand how age affects
        the value of a player. Below, we can see the distribution of the
        transfer fees of the different ages of players"),
      hr()
      )
    )
  )
)
################################################################################
ui <- fluidPage(
  navbarPage(
    "Austin Animal Shelter",
    intro_page,
    scatter_tab,
    time_tab
  )
)
