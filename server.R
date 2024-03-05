# Run them all in the R Console.
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)

trav_and_res_df <- read.csv("trav_and_res.csv") # Set work
  
  server <- function(input, output){
    
    # TODO Make outputs based on the UI inputs here
    output$barplot <- renderPlotly({
      selected_team_data <- reactive({
        if (input$team_selector == "Seattle Kraken") {
          filtered_data <- trav_and_res_df %>%
            summarize(
              Sea_wins = sum(grepl("Sea W", W.or.L)),
              Sea_losses = -sum(grepl("Sea L", W.or.L))
            )
        } else if (input$team_selector == "Pittsburgh Penguins") {
          filtered_data <- trav_and_res_df %>%
            summarize(
              Pitt_wins = sum(grepl("Pitt W", W.or.L)),
              Pitt_losses = -sum(grepl("Pitt L", W.or.L))
            )
        } else {
          filtered_data <- trav_and_res_df %>% 
            summarize(
              Pitt_wins = sum(grepl("Pitt W", W.or.L)),
              Pitt_losses = -sum(grepl("Pitt L", W.or.L)),
              Sea_wins = sum(grepl("Sea W", W.or.L)),
              Sea_losses = -sum(grepl("Sea L", W.or.L))
            )
        }
        return(filtered_data)
      })
      
      wins_losses <- reactive({
        selected_team_data() %>%
          pivot_longer(cols = everything(),
                       names_to = "Category", values_to = "Games")
      })
      
      barplot <- ggplot(wins_losses(), aes(x = Category, y = Games, fill = Category)) +
        geom_bar(stat = "identity", width = 0.7) +
        labs(title = "Kraken vs. Penguins", x = "Category", y = "Games") +
        scale_fill_manual(values = c("Pitt_wins" = "#FFB81C", "Pitt_losses" = "#CC9612",
                                     "Sea_wins" = "#75aadb", "Sea_losses" = "#4c7a9d")) + 
        theme(axis.line.x = element_line(color = "black", size = 1, linetype = "solid"))
      
      barplot <- barplot + coord_cartesian(ylim = c(-50, 50)) + 
      geom_hline(yintercept = 0, color = "black", size = 0.3)
      plotly::ggplotly(barplot)
    })
  }