library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)

trav_and_res_df <- read.csv("trav_and_res.csv")


## OVERVIEW TAB INFO

overview_tab <- tabPanel("Overview Tab Title",
   h1("Some title"),
   p("some explanation")
)

## VIZ 1 TAB INFO

viz_1_sidebar <- sidebarPanel(
  h2("Options for graph"),
  #TODO: Put inputs for modifying graph here
)

viz_1_main_panel <- mainPanel(
  h2("Vizualization 1 Title"),
  # plotlyOutput(outputId = "your_viz_1_output_id")
)

viz_1_tab <- tabPanel("Viz 1 tab title",
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
  h2("Options for graph"),
  #TODO: Put inputs for modifying graph here
)

viz_3_main_panel <- mainPanel(
  h2("Vizualization 3 Title"),
  # plotlyOutput(outputId = "your_viz_1_output_id")
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