library(shiny)
library(plotly)
library(jpeg)
library(shinyWidgets)
source("scripts/visualizations.R")
source("scripts/summary_info.R")
source("scripts/aggregate_table.R")


#Summary variables##############################################################

in_df <- read.csv(
  file = "data/aac_intakes.csv",
  encoding = "UTF-8",
  stringsAsFactor = FALSE)

out_df <- read.csv(
  file = "data/aac_outcomes.csv",
  encoding = "UTF-8",
  stringsAsFactor = FALSE)

in_out_df <- read.csv(
  file = "data/aac_intakes_outcomes.csv",
  encoding = "UTF-8",
  stringsAsFactor = FALSE)

info_in <- summary_info_in(in_df)
info_out <- summary_info_out(out_df)
info_in_out <- summary_info_in_out(in_out_df)

#Analysis Charts################################################################
cat_time <- ggplotly2(time_series(in_out_df, c("Cat", "Dog"), T))
black_cats_time <- black_cats_in_out_time(in_df, out_df)
nonblack_cats_time <- nonblack_cats_in_out_time(in_df, out_df)
out_bar <- out_bars(out_df)
in_bar <- in_bars(in_df)

#Intro##########################################################################
intro_page <- tabPanel(
  strong("Intro"),
    #div(class = "intro",
    #titlePanel(
      div(class = "title",div(class = "title_text",
      h1(strong(span(class = "border","Analysis on the"))),
      h1(strong(span(class = "border","Austin Animal Center"))),
      h3(span(class = "border","by Anthony Ngo"))
      )
      ),
    div(class = "info_content",
        h1("Purpose"),
        p("I wanted to run exploratory data analysis on a dataset related to
          something I'm passionate about. After looking around on a few portals 
          and websites, I stumbled upon a dataset on Kaggle about the Austin 
          Animal Center, a great No-Kill animal shelter based in Austin, Texas."),
        p("This dataset worked wonderfully for what I wanted to do. The dataset
          is well documented and has sufficient observations. More importantly,
          however, I'm interested in what such data could imply or reveal. I'm
          an advocate for adopting pets from animal shelters and pounds
          as opposed to from breeders and puppy mills. According to the ",
          a(href = "https://www.americanpetproducts.org/",
            target = "_blank",
            strong("American Pet Products Association")),
          "of the 44% of households that own dogs, 34% of those households 
          adopted their dog from breeders while only 23% of those households
          adopted their dog from either an animal shelter or human society.
          Although people are allowed to adopt from wherever and whomever they
          wish to adopt from, I believe that adopting from shelters, rescues, 
          and humane societies is more beneficial to both pets and owners alike
          and more people should adopt from shelters rather than from breeders."
          
          ),
        p("Additionally, according to the",
          a(href = "https://www.aspca.org/",
            target = "_blank",
            strong("American Society for the Prevention of Cruelty to Animals")),
          " about 3.2 million shelter animals are adopted each year. However,",
          em("6.5 million"), " pets are taken into animal shelters each year
            and around ", em("1.5 million"), " are euthanized annually. Although
            these numbers have declined since 2011, when there were 7.2 million
            animals entering animal shelters and 2.6 million animals being
            euthanized each year, I believe we should strive to decrease these
            statistics until the number of adoptions exceed the number of animals
            coming into animal shelters and the number of euthanized pets reaches
            0."),
        p("I want to explore the popularity of certain dog breeds and how
          breed affects a dog's experience in a shelter. Pitbulls, for example,
          tend to be seen as fairly aggressive and dangerous dogs, so I want to
          explore how a Pitbull's time in a shelter differs from perhaps a 
          Dachshund's. I also want to explore how adoption rates change in 
          relation to other characteristics such as Age, Sex, or even month
          of the year."),
        p("In addition to my own analysis, I want to provide tools that others
          could use to conduct their own analysis and find other patterns and
          trends."),
        
        h1("What is the Austin Animal Center?"),
        p("The", a(href = "http://www.austintexas.gov/department/aac",
                   target = "_blank",
          strong("Austin Animal Center")), "is the largest No-Kill Animal Shelter
          in the United States. The center provides shelter to over 16,000 
          animals per year as well as animal protection and other pet resource
          services to the Austin and Travis Counties of Texas State. The Austin
          Animal Center is an open-intake facility that accepts all lost and
          surrendered animals from all of Travis County in need of shelter
          regardless of age, health, species or breed with a goal to \"place all
          adoptable animals in forever homes\". Since March of 2010, the Center
          has saved ", em("over 90%"), " of the animals in their shelter and has
          been providing government-funded neutering and spaying surgeries,
          vaccinations, microchipping, an more under Austin's No-Kill
          implementation plan."),
        
        h1("The Data"),
        p("I obtained this data from ", 
          a(href = "https://www.kaggle.com/aaronschlegel/austin-animal-center-
            shelter-intakes-and-outcomes",
            target = "_blank",
            strong("Kaggle")),
          "on the 27th of July, 2019."),
        p("Aaron Schlegel, a data analyst based in Seattle, Washington, maintains
          the data. However, Schelgel derived it from",
          a(href = "https://data.austintexas.gov/",
            target = "_blank",
            strong("the Official City of Austin Open Data Portal")),
            ". Although the data from the portal is updated regularly, Schlegel's
              data was last updated in April of 2018. Schlegel's dataset utilizes
          two datasets from the Austin Portal (Animal Shelter Outcomes and Animal
          Shelter Intakes) to create a third dataset for animals that the Austin
          Animal Shelter both took in and released within the timeframe between 
          October 1st of 2013 and April of 2018. This third dataset is what most
          of my analysis is based on."
          ),
        h2("Preliminary Analysis"),
        p("Between October of 2013 and April of 2018, there were ",
          paste(formatC(info_in$in_count, big.mark=',')),
          " animals taken in by the Austin Animal Center and",
          paste(formatC(info_out$out_count, big.mark=',')), " animals that left
          the shelter. Of these two groups, there were ", 
          paste(formatC(info_in_out$in_out_count,big.mark=',')),
          " animals that both entered and left the Austin Animal Center
          within October of 2013 and April of 2018"),
        p("I found that the type of animal that is most likely to be adopted
          is a ", tolower(info_out$most_adopted_animal), " with an adoption
          rate of ", info_out$most_adopted_perc, "% while the type of animal
          that is most likely to be euthanized is ",
          tolower(info_out$most_euthanized_animal), " with a euthanasia rate
          of ", info_out$most_euthanized_perc, "% (This group includes animals
          that aren't don't fall into one of the larger categories such as 
          bats, hamsters, and rabbits)"),
        p("The type of animal that is most frequently taken in by the AAC is 
          a ", tolower(info_in$pop_animal_in), ".",
          "There was one animal that was more frequently taken in by the AAC
          than any other animal: a ",tolower(info_in$freq_case_sex), " ",
          tolower(info_in$freq_case_breed), " named ", info_in$freq_case_name,
        " who was taken into the Austin Animal Shelter ", 
        em(info_in$freq_case_count, " different times.")),
        p("Of all animals that entered and left the AAC between October of 2013
          and April of 2018, the average number of days spent on the shelter is
          ", round(info_in_out$avg_time_in_shelter_days),
          " days. The type of animal that spends the least amount of time in the
          shelter is ",
          tolower(info_in_out$shortest_avg_time_animal), "with an average of ",
          round(info_in_out$shortest_avg_time) ,
          "days in the AAC, while the type of animal that tends to spend the 
          greatest number of days in the shelter is a ",
          tolower(info_in_out$longest_avg_time_animal),
          "with an average of ", round(info_in_out$longest_avg_time), " days."),
        p("The average age of an animal coming into the AAC is ",
          round(info_in_out$avg_age_in, 2),
          " years old while the average of an animal coming out of the AAC is ",
          round(info_in_out$avg_age_out, 2), 
          " years old."),
        
        h2("Aggregate Table"),

        dataTableOutput("table"),
        
        br()
    )
    )
  #)

#scatter_tab####################################################################
scatter_tab <- tabPanel(
  strong("Pet Explorer"),
  div(class = "content",
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      div(class = "side",h2(strong("Control Panel")),
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
        h1("Pet Explorer"),
        hr(),
        plotlyOutput("p1")
      )
    )
  ))
)

#time_tab#######################################################################
time_tab <- tabPanel(
  strong("Adoptions over Time"),
  div(class = "content",
  sidebarLayout(
    sidebarPanel = sidebarPanel(div(class = "side",
      h2(strong("Control Panel")),
      hr(),
      h3("Separate Lines"),
      switchInput("sep", label = "", value = T),
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
      h1("Adoptions over Time"),
      hr(),
      plotlyOutput("p2")
      )
    )
  )
  )
)

#radar_tab######################################################################
radar_tab <- tabPanel(
  strong("Dogs Vs Cats"),
  div(class = "content",
  sidebarLayout(
    sidebarPanel = sidebarPanel(div(
      h2(strong("Control Panel")),
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
                          h1("Dogs vs Cats"),
                          hr(),
                          plotlyOutput("p3_dog"),
                          plotlyOutput("p3_cat")
                          )
  )
  )
)
#analysis_tab###################################################################
analysis_tab <- tabPanel(
  strong("Analysis"),
  div(class = "info_content",
      br(),
      br(),
      h1("Analysis"),
      p("Exploratory Analysis on the the Austin Animal Shelter Dataset using R.
        The Austin Animal Center is the largest no-kill animal shelter in the 
        United States that provides care and shelter to over 18,000 animals 
        each year. The time frame for all visualizations is from 2013 Q4 to
        2018 Q1."),
      ##########################################################################      
      h2("Cat Adoptions over Time"),br(),
      cat_time,
      br(),hr(),br(),
      p("I found that dog adoptions tend to be more frequent than Cat adoptions
        most of any given year. However, for one month of the year, the number
        of cat adoptions is exceeds the number of dog adoptions. Every July,
        there's a large spike in the frequency of cat adoptions while the 
        number of dog adoptions tends to be more consistent over the year.
        Initially, I felt that this spike was because of some psychological or
        social behavior that makes cats more popular. Perhaps people yearn
        more for feline companions around this time of the year. However, after
        looking into the ",
        a(href = "https://www.facebook.com/AustinAnimalCenter/",
          target = "_blank",
          strong("Austin Animal Center's Facebook page")), 
        "and their activity history, I realize that this trend is better 
        accredited to the events that the AAC hosts around July every year that
        is focused on promoting feline adoption such as their annual 
        KittyPalooza! and 2019's Hakuna MaCATa."),
      p("These events seem to be very effective, making July have an adoption
        frequency ", em("30% greater on average"), " than the month with the
        second highest adoption count. Furthermore, the event seems to have a
        lingering effect as well. The number of cat adoptions doesn't just spike 
        for the month of July and immediately fall back down the very next month.
        Starting in March or April of every year, the number of cat adoptions
        gradually and consistently increases until July. After July, the number
        of cat adoptions slowly fall back down. That is, until around the months
        of October and November."
        ), 
      p("Around October and November of every year, there is a slight increase
        in cat adoptions. Although there are events around this time that 
        promote the adoption of feline companions, this trend occurred in years
        where there were no events promoting cats. At least, this trend isn't 
        easily accredited to any event that ",
        em("the Austin Animal Center specifically hosts."),
        "I attribute this slight jump in adoption frequency to Halloween.
        There's been a cliche about Satanic Cults adopting cats around this time
        to be used in demonic rituals and sacrifices. Although this is a dark 
        and ominous theory that fits in well with the Halloween theme, according
        to ",
        a(href = "https://www.snopes.com/fact-check/cat-o-nine-tales/",
          target = "_blank",
          strong("Snopes")),
        ", this is very much a myth. Nevertheless, cat adoptions are still
        increased during the time of Halloween. This idea surrounding Satanic
        Cults adopting cats still contribute to the increase of feline adoptions
        around this time. Advertisements for Halloween using this trope 
        inadvertently promotes cats as well."
        ),
      ##########################################################################
      h2("A Black Cat's Year"),br(),
      black_cats_time,
      br(),hr(),br(),
      nonblack_cats_time,
      br(),hr(),br(),
      p("After looking further the relationship between cat adoptions and 
        Halloween, I wanted to start looking into black cats specifically. Black
        cats are generally less popular than other colors of felines. One reason
        for this is the superstition of black cats being \"unlucky\". 
        Furthermore, many shelters specifically forbids the adoption of black
        cats around the time of Halloween in order to both prevent the myth of 
        satanic rituals as well as to deter people who wish to adopt black cats
        simply as Halloween \"props\". However, I found that this isn't 
        necessarily the case at the Austin Animal Center. In fact, black cats
        have been recently increasing in popularity. Both black cats and
        non-black cats share identical yearly patterns and identical ratios of
        adoptions to owner surrenders (usually about 4:1). "
        ),
      ##########################################################################
      h2("Pit Bulls vs Other Breeds"),
      p("In this analysis, we'll be considering pit bulls as both purebred pit
        bulls as well as pit bull mixes"),br(),
      out_bar,
      br(),hr(),br(),
      p("Pit bulls tend to have a reputation for being aggressive. Potential 
        owners tend to see this breed of canine as dangerous and belligerent and
        therefore avoid them as pets. The rate of adoption for pit bulls as 
        opposed to other dog species reflects this aversion. Pit bulls and 
        pitbull mixes have an adoption rate 10% lower than the average dog. Of
        all pit bulls leaving the Austin Animal Shelter, 37% are adopted while 
        47% of other species are adopted. Furthermore, the proportion of pit
        bulls that are euthanized (0.07) is twice the proportion of other
        dog breeds(0.03). However, owners who ", em("have"), " adopted pit bulls
        tend to be more attached to their animal than owners of other dog 
        breeds. Pit bulls who come into the AAC, whether through being found in
        the streets or being surrendered by their owner, are returned to their 
        owner at a proportion of of 0.35. This implies that pit bull owners are
        more likely usually to want to re-adopt their pit bull after being
        detached from him/her for some time more compared to owners of other
        breeds. Other dog breeds have a return to owner proportion of 0.28."
      ),br(),
      in_bar,
      br(),hr(),br(),
      p("Also supporting the idea that pit bull owners are more attached to 
        their pets is that pit bulls are less likely to be found as a stray or 
        to be surrendered to the Austin Animal Shelter. This implies that pit 
        bulls are less likely to be abandoned/lost by their owners compared to 
        other dog breeds and when they are, they're more likely to be returned to 
        their original owners too."),
      h2("Are Pitbulls Aggressive?"),
      br(),
      br()
                             ))
#UI#############################################################################
ui <- fluidPage(
  includeCSS("www/intro.CSS"),
  navbarPage(
    a(href = "http://anthonydinhngo.github.io",
      style = "text-decoration: none;",
      img(src = 'Icon_4_white.png')
      ),id = "navbar",collapsable=F,
    intro_page,
    scatter_tab,
    time_tab,
    radar_tab,
    analysis_tab
  )
)
