library(shiny)
library(plotly)


#Intro##########################################################################
intro_page <- tabPanel(
  "Intro",
  titlePanel("Intro page"),
  em("[Intro Placeholder]")
  )

#scatter_tab####################################################################
scatter_tab <- tabPanel(
  "Scatterplot",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      div(class = "side",h2("Control Panel"),
                                hr(),
      checkboxGroupInput("animal_list", label = h3("Animal Type"), 
                         choices = list("Dog" = "Dog", "Cat" = "Cat", "Bird" = "Bird", "Other" = "Other"),
                         selected = "Dog"),
      sliderInput("years", label = h3("Years"), min = 2013, 
                  max = 2018, value = c(2015, 2016)),
      hr(),
      selectInput("out", label = h3("Outcome"), 
                  choices = list("Adoption" = "Adoption",
                                 "Return to Owner" = "Return to Owner",
                                 "Transfer" = "Transfer",
                                 "Euthanasia" = "Euthanasia",
                                 "Died"), 
                  selected = "Adoption")
      )
      
    ),
    mainPanel = mainPanel(
      div(
        class = "main",
        p("With this table, we want to be able to understand how age affects
            the value of a player. Below, we can see the distribution of the
            transfer fees of the different ages of players"),
        hr(),
        plotlyOutput("p1")
      )
    )
  )
)

#time_tab#######################################################################
time_tab <- tabPanel(
  "Time Series",
  sidebarLayout(
    sidebarPanel = sidebarPanel(div(class = "side",
      h2("Control Panel"),
      hr(),
      checkboxInput("sep", label = "Separate Lines", value = T),
      hr(),
      checkboxGroupInput("animal_list_time", label = h3("Animal Type"), 
                         choices = list("Dog" = "Dog", "Cat" = "Cat", "Bird" = "Bird", "Other" = "Other"),
                         selected = "Dog")
    )
    )
  ,
  mainPanel = mainPanel(
    div(
      class = "main",
      p("With this table, we want to be able to understand how age affects
        the value of a player. Below, we can see the distribution of the
        transfer fees of the different ages of players"),
      hr(),
      plotlyOutput("p2")
      )
    )
  )
)

#radar_tab######################################################################
radar_tab <- tabPanel(
  "Dog Radar",
  sidebarLayout(
    sidebarPanel = sidebarPanel(div(
      h2("Control Panel"),
      radioButtons("outcome_intake", label = h3("Radar Properties"),
                   choices = list("Intakes" = F, "Outcomes" = T), 
                   selected = F),
      hr(),
      selectInput("select_dog1", label = h3("1st Dog Breed"), 
                  choices = list("All" = "All",
                                 "Pit Bull Mix" = "Pit Bull Mix",
                                 "Chihuahua Shorthair Mix" = "Chihuahua Shorthair Mix",
                                 "Labrador Retriever Mix" = "Labrador Retriever Mix",
                                 "German Shepherd Mix" = "German Shepherd Mix",
                                 "Australian Cattle Dog Mix" = "Australian Cattle Dog Mix",
                                 "Dachshund Mix" = "Dachshund Mix",
                                 "Boxer Mix" = "Boxer Mix",
                                 "Border Collie Mix" = "Border Collie Mix",
                                 "Miniature Poodle Mix" = "Miniature Poodle Mix",
                                 "Catahoula Mix" = "Catahoula Mix",
                                 "Rat Terrier Mix" = "Rat Terrier Mix",
                                 "Australian Shepherd Mix" = "Australian Shepherd Mix",
                                 "Yorkshire Terrier Mix" = "Yorkshire Terrier Mix",
                                 "Siberian Husky Mix" = "Siberian Husky Mix",
                                 "Jack Russell Terrier Mix"  = "Jack Russell Terrier Mix",
                                 "Miniature Schnauzer Mix" = "Miniature Schnauzer Mix",
                                 "Beagle Mix" = "Beagle Mix",
                                 "Staffordshire Mix" = "Staffordshire Mix",
                                 "Chihuahua Longhair Mix" = "Chihuahua Longhair Mix",
                                 "Great Pyrenees Mix" = "Great Pyrenees Mix",
                                 "Cairn Terrier Mix" = "Cairn Terrier Mix",
                                 "Pointer Mix" = "Pointer Mix",
                                 "Rottweiler Mix" = "Rottweiler Mix",
                                 "American Bulldog Mix" = "American Bulldog Mix",
                                 "Shih Tzu Mix"  = "Shih Tzu Mix"),
                  selected = "All"),
      hr(),
      selectInput("select_dog2", label = h3("2nd Dog Breed"), 
                  choices = list("All" = "All",
                                 "Pit Bull Mix" = "Pit Bull Mix",
                                 "Chihuahua Shorthair Mix" = "Chihuahua Shorthair Mix",
                                 "Labrador Retriever Mix" = "Labrador Retriever Mix",
                                 "German Shepherd Mix" = "German Shepherd Mix",
                                 "Australian Cattle Dog Mix" = "Australian Cattle Dog Mix",
                                 "Dachshund Mix" = "Dachshund Mix",
                                 "Boxer Mix" = "Boxer Mix",
                                 "Border Collie Mix" = "Border Collie Mix",
                                 "Miniature Poodle Mix" = "Miniature Poodle Mix",
                                 "Catahoula Mix" = "Catahoula Mix",
                                 "Rat Terrier Mix" = "Rat Terrier Mix",
                                 "Australian Shepherd Mix" = "Australian Shepherd Mix",
                                 "Yorkshire Terrier Mix" = "Yorkshire Terrier Mix",
                                 "Siberian Husky Mix" = "Siberian Husky Mix",
                                 "Jack Russell Terrier Mix"  = "Jack Russell Terrier Mix",
                                 "Miniature Schnauzer Mix" = "Miniature Schnauzer Mix",
                                 "Beagle Mix" = "Beagle Mix",
                                 "Staffordshire Mix" = "Staffordshire Mix",
                                 "Chihuahua Longhair Mix" = "Chihuahua Longhair Mix",
                                 "Great Pyrenees Mix" = "Great Pyrenees Mix",
                                 "Cairn Terrier Mix" = "Cairn Terrier Mix",
                                 "Pointer Mix" = "Pointer Mix",
                                 "Rottweiler Mix" = "Rottweiler Mix",
                                 "American Bulldog Mix" = "American Bulldog Mix",
                                 "Shih Tzu Mix"  = "Shih Tzu Mix"),
                  selected = "Pit Bull Mix"),
      hr(),
      selectInput("select_cat1", label = h3("1st Cat Breed"), 
                  choices = list("All" = "All",
                                 "Domestic Shorthair Mix" = "Domestic Shorthair Mix",
                                 "Domestic Medium Hair Mix" = "Domestic Medium Hair Mix",
                                 "Domestic Longhair Mix" = "Domestic Longhair Mix",
                                 "Siamese Mix" = "Siamese Mix",
                                 "Domestic Shorthair" = "Domestic Shorthair",
                                 "American Shorthair Mix" = "American Shorthair Mix",
                                 "Snowshoe Mix" = "Snowshoe Mix",
                                 "Domestic Medium Hair" = "Domestic Medium Hair",
                                 "Maine Coon Mix" = "Maine Coon Mix",
                                 "Manx Mix" = "Manx Mix",
                                 "Siamese" = "Siamese",
                                 "Russian Blue Mix" = "Russian Blue Mix",
                                 "Domestic Longhair" = "Domestic Longhair",
                                 "Himalayan Mix" = "Himalayan Mix",
                                 "Ragdoll Mix" = "Ragdoll Mix",
                                 "Persian Mix" = "Persian Mix",
                                 "Siamese/Domestic Shorthair" = "Siamese/Domestic Shorthair",
                                 "Bengal Mix" = "Bengal Mix",
                                 "Angora Mix" = "Angora Mix",
                                 "American Curl Shorthair Mix" = "American Curl Shorthair Mix",
                                 "Maine Coon" = "Maine Coon",
                                 "Balinese Mix" = "Balinese Mix",
                                 "Japanese Bobtail Mix" = "Japanese Bobtail Mix",
                                 "Persian" = "Persian",
                                 "Tonkinese Mix" = "Tonkinese Mix"),
                  selected = "All"),
      hr(),
      selectInput("select_cat2", label = h3("2nd Cat Breed"), 
                  choices = list("All" = "All",
                                 "Domestic Shorthair Mix" = "Domestic Shorthair Mix",
                                 "Domestic Medium Hair Mix" = "Domestic Medium Hair Mix",
                                 "Domestic Longhair Mix" = "Domestic Longhair Mix",
                                 "Siamese Mix" = "Siamese Mix",
                                 "Domestic Shorthair" = "Domestic Shorthair",
                                 "American Shorthair Mix" = "American Shorthair Mix",
                                 "Snowshoe Mix" = "Snowshoe Mix",
                                 "Domestic Medium Hair" = "Domestic Medium Hair",
                                 "Maine Coon Mix" = "Maine Coon Mix",
                                 "Manx Mix" = "Manx Mix",
                                 "Siamese" = "Siamese",
                                 "Russian Blue Mix" = "Russian Blue Mix",
                                 "Domestic Longhair" = "Domestic Longhair",
                                 "Himalayan Mix" = "Himalayan Mix",
                                 "Ragdoll Mix" = "Ragdoll Mix",
                                 "Persian Mix" = "Persian Mix",
                                 "Siamese/Domestic Shorthair" = "Siamese/Domestic Shorthair",
                                 "Bengal Mix" = "Bengal Mix",
                                 "Angora Mix" = "Angora Mix",
                                 "American Curl Shorthair Mix" = "American Curl Shorthair Mix",
                                 "Maine Coon" = "Maine Coon",
                                 "Balinese Mix" = "Balinese Mix",
                                 "Japanese Bobtail Mix" = "Japanese Bobtail Mix",
                                 "Persian" = "Persian",
                                 "Tonkinese Mix" = "Tonkinese Mix"),
                  selected = "Domestic Shorthair Mix")
    )
    ),
    mainPanel = mainPanel(class = "main",
                          p("Content"),
                          plotlyOutput("p3_dog"),
                          plotlyOutput("p3_cat")
                          )
  )
)
#UI#############################################################################
ui <- fluidPage(
  includeCSS("www/intro.CSS"),
  navbarPage(
    htmlOutput("banner_pic"),id = "navbar",collapsable=F,
    intro_page,
    scatter_tab,
    time_tab,
    radar_tab
  )
)
