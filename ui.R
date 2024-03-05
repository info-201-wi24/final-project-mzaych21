library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
<<<<<<< HEAD
library(shiny)
=======
library(maps)
>>>>>>> f04815ae81d3d26c8fa7b17d173543fe72c7f77c

trav_and_res_df <- read.csv("trav_and_res.csv")


## OVERVIEW TAB INFO

overview_tab <- tabPanel("Overview Tab Title",
                         h1("Some title"),
                         p("some explanation")
)

## VIZ 1 TAB INFO

viz_1_sidebar <- sidebarPanel(
  h2("Choose Your Team!"),
  selectInput(
    inputId = "team_selector1",
    label = "Select a Team:",
    choices = c("Pittsburgh Penguins", "Seattle Kraken", "Both"),
    selected = "Both"
  )
)

viz_1_main_panel <- mainPanel(
  h2("US Map where states with at least one NHL Arena are colored in"),
  plotlyOutput(outputId = "mapplot")
)

viz_1_tab <- tabPanel("Viz 1 tab title",
                      sidebarLayout(
                        viz_1_sidebar,
                        viz_1_main_panel
                      )
)

## VIZ 2 TAB INFO
## Basic bar graph of total wins for the two teams
<<<<<<< HEAD
## Also, we could have 4 bars, Pitt W and Pitt L and Seattle W and Seattle L. 
=======
## Also, we could have 4 bars, Pitt W and Pitt L and Seattle W and Seattle L.
>>>>>>> f04815ae81d3d26c8fa7b17d173543fe72c7f77c

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
  )
)


viz_2_main_panel <- mainPanel(
  h2("Total Wins and Losses"),
  # plotlyOutput(outputId = "your_viz_1_output_id")
  plotlyOutput(outputId = "barplot")
)

viz_2_tab <- tabPanel("Viz 2 tab title",
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
  )
)

viz_3_main_panel <- mainPanel(
  h2("Vizualization 3 Title"),
  plotlyOutput(outputId = "lineplot")
)

viz_3_tab <- tabPanel("Viz 3 tab title",
                      sidebarLayout(
                        viz_3_sidebar,
                        viz_3_main_panel
                      )
)

## CONCLUSIONS TAB INFO

conclusion_tab <- tabPanel("Conclusion Tab Title",
                           h1("Some title"),
                           p("some conclusions")
)



ui <- navbarPage("Example Project Title",
                 overview_tab,
                 viz_1_tab,
                 viz_2_tab,
                 viz_3_tab,
                 conclusion_tab
)
