library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(maps)
library(bslib)


my_theme <- bs_theme(version = 4, bootswatch = "cosmo")


trav_and_res_df <- read.csv("trav_and_res.csv")


## OVERVIEW TAB INFO

overview_tab <- tabPanel("Introduction and Background",
                         tags$img(src = "https://kubrick.htvapps.com/htv-prod-media.s3.amazonaws.com/images/penguins-seattle-kraken-1638806374.jpg?crop=1.00xw:1.00xh;0,0&resize=1200:*", height = "475px"),
                         h1("Introduction"),
                         p("The major question we are answering is how much more does a team from the Western Conference of the National Hockey League travel in comparison to a team from the Eastern Conference. In addition, we are hoping to see if there is a correlation between this travel and in-game success. The data came from Google Maps which showed how far each team traveled in miles from one arena to another and our Team and game results came from fixture downloads."), 
                         p("There were no ethical limitations as the results of hockey games don’t have a bias – one team must win and the other must lose. However, something to note is we did not know the exact flight path, what hotel, and what driving routes the teams took. In addition, we didn’t know if a team would drive to a nearby arena like New York to New Jersey for example. Another example could be Pittsburgh going straight from a city like San Jose to Anaheim for back-to-back games instead of returning home in between."), 
                         p("To combat this logistical issue we just decided to take the distance in between arenas in miles, and assumed for every away game they left directly from their home arena. We were forced to improvise due to our lack of access to the team's travel routes. So for the purpose of this assignment, and due to knowing the exact travel routes we always took distances from the home arena and calculated accordingly."), 
                         p("Links: https://fixturedownload.com/results/nhl-2022/seattle-kraken, https://fixturedownload.com/results/nhl-2022/pittsburgh-penguins")
)

## VIZ 1 TAB INFO

viz_1_sidebar <- sidebarPanel(
  h2("Choose Your Team!"),
  selectInput(
    inputId = "team_selector1",
    label = "Select a Team:",
    choices = c("Pittsburgh Penguins", "Seattle Kraken", "Both"),
    selected = "Both"
  ),
  span(textOutput("message1"), style = "color:green"),
  span(textOutput("message2"), style = "color:gold"),
  span(textOutput("message3"), style = "color:orange"),
  span(textOutput("message4"), style = "color:red"),
  h2("Analysis:"),
  span(textOutput("description"))
  
)

viz_1_main_panel <- mainPanel(
  h2("US Map where states with at least one NHL Arena are colored in"),
  plotlyOutput(outputId = "mapplot")
)

viz_1_tab <- tabPanel("US Map With Arena and States",
                      sidebarLayout(
                        viz_1_sidebar,
                        viz_1_main_panel
                      )
)

## VIZ 2 TAB INFO
## Basic bar graph of total wins for the two teams
## Also, we could have 4 bars, Pitt W and Pitt L and Seattle W and Seattle L.

viz_2_sidebar <- sidebarPanel(
  h2("Choose Your Team!"),
  selectInput(
    inputId = "team_selector",
    label = "Select a Team:",
    choices = c("Pittsburgh Penguins", "Seattle Kraken", "Both"),
    selected = "Both"
  ),
  conditionalPanel(
    condition = "input.team_selector == 'Pittsburgh Penguins'",
    tags$div(
      h4("Percentage:"),
      tags$p("Pitt wins: 50%"),
      tags$p("Pitt losses: 50%"),
      tags$p("Out of total 82 games")
    )
  ),
  conditionalPanel(
    condition = "input.team_selector == 'Seattle Kraken'",
    tags$div(
      h4("Percentage:"),
      tags$p("Sea wins: 43%"),
      tags$p("Sea losses: 57%"),
      tags$p("Out of total 82 games")
    )
  ),
  conditionalPanel(
    condition = "input.team_selector == 'Both'",
    tags$div(
      h4("Percentage:"),
      tags$p("Sea wins: 43%"),
      tags$p("Sea losses: 57%"),
      tags$p("Pitt wins: 50%"),
      tags$p("Pitt losses: 50%"),
      tags$p("Out of total 82 games")
    )
  ),
  h2("Analysis:"),
  span(textOutput("description2"))
  
)


viz_2_main_panel <- mainPanel(
  h2("Total Wins and Losses"),
  # plotlyOutput(outputId = "your_viz_1_output_id")
  plotlyOutput(outputId = "barplot")
)

viz_2_tab <- tabPanel("Team Wins and Losses",
                      sidebarLayout(
                        viz_2_sidebar,
                        viz_2_main_panel
                      )
)

## VIZ 3 TAB INFO

viz_3_sidebar <- sidebarPanel(
  h2("Choose Your Team!"),
  selectInput(
    inputId = "team_selector3",
    label = "Select a Team:",
    choices = c("Pittsburgh Penguins", "Seattle Kraken", "Both"),
    selected = "Both"
  ), 
  conditionalPanel(
    condition = "input.team_selector3 == 'Pittsburgh Penguins'",
    tags$div(
      h4("Percentage:"),
      tags$p("Pitt wins: 43%"),
      tags$p("Pitt losses: 57%"),
      tags$p("Out of total 41 away games")
    )
  ),
  conditionalPanel(
    condition = "input.team_selector3 == 'Seattle Kraken'",
    tags$div(
      h4("Percentage:"),
      tags$p("Sea wins: 61%"),
      tags$p("Sea losses: 39%"),
      tags$p("Out of total 41 away games")
    )
  ),
  conditionalPanel(
    condition = "input.team_selector3 == 'Both'",
    tags$div(
      h4("Percentage:"),
      tags$p("Sea wins: 61%"),
      tags$p("Sea losses: 39%"),
      tags$p("Pitt wins: 43%"),
      tags$p("Pitt losses: 57%"),
      tags$p("Out of total 41 away games")
    )
  ),
  h2("Analysis:"),
  span(textOutput("description3"))
  
)


viz_3_main_panel <- mainPanel(
  h2("Away Games"),
  plotlyOutput(outputId = "scatterplot")
  
)

viz_3_tab <- tabPanel("Away Game Travel",
                      sidebarLayout(
                        viz_3_sidebar,
                        viz_3_main_panel
                      )
)

## CONCLUSIONS TAB INFO

conclusion_tab <- tabPanel("Closing",
                           h1("Conclusion/Summary"),
                           p("After carefully analyzing the data several key takeaways were noted from our project.
First, although not all 32 teams were analyzed, our comparison of the
Seattle Kraken and Pittsburgh Penguins makes it evident that traveling less does not always
lead to a team winning more games. This is seen in our bar graph as the Seattle Kraken had a heavier travel schedule but won more games than the Penguins (47 wins compared to
41)."), p("Additionally, we also noticed that there is a heavier concentration of Arenas on the
east coast. We see that in the Northeast corner of the United States of America, it is
significantly more dense in terms of Arenas to land, while the west coast is more
spread out. This is a key reason as to why the travel load for a team like the Pittsburgh
The Penguins are going to be much less than a team like the Seattle Kraken."), 
                           p("Lastly, it is evident that 
Pittsburgh traveled less as on the line plot of travel distance we can see that if the sum of total 
travel in miles is taken, Pittsburgh had significantly less. However, it is important to note that 
this information needs to be taken lightly as mentioned in the intro, since we are not sure of the exact 
flight path, hotel accommodations, and road paths."), p("Our mileage was calculated based on the distance exactly from one arena to the other in miles rather than the exact travel plan that the team followed. Overall however, it is evident that traveling less doesn’t always mean having a more fortuitous season, that the NHL is more densely populated with arenas in the Northeast portion of the United States, and that Pittsburgh had a significantly less invasive travel schedule than the Seattle Kraken."))



ui <- navbarPage("Do Travel and Wins Go Hand and Hand in the NHL",
                 theme = my_theme,
                 overview_tab,
                 viz_1_tab,
                 viz_2_tab,
                 viz_3_tab,
                 conclusion_tab
               
)
