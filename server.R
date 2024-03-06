
trav_and_res_df <- read.csv("trav_and_res.csv")  # set working directory to source file location.
pitt_df <- trav_and_res_df %>% filter(Sea.or.Pitt == "Pitt") %>% arrange(distance.from.arena.in.miles) %>% mutate(row = row_number())
sea_df <- trav_and_res_df %>% filter(Sea.or.Pitt == "Sea") %>% arrange(distance.from.arena.in.miles) %>% mutate(row = row_number())
sorted_df <- trav_and_res_df %>% arrange(distance.from.arena.in.miles) %>% mutate(row = row_number())



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
    #plotly::ggplotly(barplot)
    return(ggplotly(barplot))
  })
  
  output$mapplot <- renderPlotly({
    
    us_states <- map_data("state")
    
    
    
    states_with_arena <- subset(us_states, region %in% c("arizona", 
                                                         "california", 
                                                         "colorado", 
                                                         "florida", 
                                                         "illinois", 
                                                         "massachusetts", 
                                                         "missouri", 
                                                         "nevada", 
                                                         "new jersey", 
                                                         "new york", 
                                                         "north carolina", 
                                                         "ohio", 
                                                         "pennsylvania", 
                                                         "tennessee", 
                                                         "texas", 
                                                         "washington", 
                                                         "district of columbia"))
    
    is_sea <- subset(us_states, region %in% c("washington"))
    
    far_from_sea <- subset(us_states, region %in% c("texas", 
                                                    "missouri", 
                                                    "illinois"))
    
    close_to_sea <- subset(us_states, region %in% c("arizona", 
                                                    "california", 
                                                    "colorado", 
                                                    "nevada"))
    
    very_far_from_sea <- subset(us_states, region %in% c("florida", 
                                                         "massachusetts", 
                                                         "new jersey", 
                                                         "new york", 
                                                         "north carolina", 
                                                         "ohio", 
                                                         "pennsylvania", 
                                                         "tennessee", 
                                                         "district of columbia"))
    
    is_pitt <- subset(us_states, region %in% c("pennsylvania"))
    
    far_from_pitt <- subset(us_states, region %in% c("texas", 
                                                     "colorado", 
                                                     "arizona"))
    
    close_to_pitt <- subset(us_states, region %in% c("new york", 
                                                     "new jersey", 
                                                     "district of columbia", 
                                                     "ohio",
                                                     "illinois",
                                                     "missouri",
                                                     "tennessee",
                                                     "north carolina",
                                                     "florida"))
    
    very_far_from_pitt <- subset(us_states, region %in% c("california", 
                                                          "nevada",
                                                          "washington"))
    
    output$message1 <- renderText({"Green: Same State"})
    output$message2 <- renderText({"Yellow: Close"})
    output$message3 <- renderText({"Orange: Far"})
    output$message4 <- renderText({"Red: Very Far"})
    output$description <- renderText({"It's evident that the north-east side of the United States is more densely populated with NHL
       arenas, proving that it is easier and more manageable for teams to travel on the east coast."})
    output$description2 <- renderText({"It is clear that the Seattle Kraken won more games than the Pittsburgh Penguins."})
    output$description3 <- renderText({"Based on the data, the Seattle Kraken traveled much more while still maintaining a better overall 
      road game win percentage."})
    
    
    
    if (input$team_selector1 == "Seattle Kraken") {
      mapplot <- ggplot() +
        geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "aliceblue", color = "black") +
        geom_polygon(data = is_sea, aes(x = long, y = lat, group = group), fill = "darkseagreen2") +
        geom_polygon(data = close_to_sea, aes(x = long, y = lat, group = group), fill = "yellow") +
        geom_polygon(data = far_from_sea, aes(x = long, y = lat, group = group), fill = "orange") +
        geom_polygon(data = very_far_from_sea, aes(x = long, y = lat, group = group), fill = "red") +
        labs(title = "Distance From Seattle") 
    } else if (input$team_selector1 == "Pittsburgh Penguins") {
      mapplot <- ggplot() +
        geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "aliceblue", color = "black") +
        geom_polygon(data = is_pitt, aes(x = long, y = lat, group = group), fill = "darkseagreen2") +
        geom_polygon(data = close_to_pitt, aes(x = long, y = lat, group = group), fill = "yellow") +
        geom_polygon(data = far_from_pitt, aes(x = long, y = lat, group = group), fill = "orange") +
        geom_polygon(data = very_far_from_pitt, aes(x = long, y = lat, group = group), fill = "red") +
        labs(title = "Distance From Pittsburgh") 
    } else {
      mapplot <- ggplot() +
        geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "aliceblue", color = "black") +
        geom_polygon(data = states_with_arena, aes(x = long, y = lat, group = group), fill = "darkseagreen2") +
        labs(title = "Neutral Plot of All States") 
    }
    
    
    return(ggplotly(mapplot))
  })
  
  Sea_away_df <- sea_df %>%
    filter(Away.Team == "Seattle Kraken") %>%
    summarize(distance.from.arena.in.miles, W.or.L) %>% 
    mutate(nth_away_games = row_number())
  
  Pitt_away_df <- pitt_df %>%
    filter(Away.Team == "Pittsburgh Penguins") %>%
    summarize(distance.from.arena.in.miles, W.or.L) %>% 
    mutate(nth_away_games = row_number())
  
  
  output$scatterplot <- renderPlotly({
    
    if (input$team_selector3 == "Seattle Kraken") {
      scatterplot <- ggplot(Sea_away_df, aes(x = nth_away_games, y = distance.from.arena.in.miles, color = W.or.L, text = W.or.L)) +
        geom_point() +
        scale_color_manual(values = c("Sea W " = "#85c1ff", "Sea L" = "#416481")) +
        labs(title = "Travel Distance and n-th Away Games")
    } else if (input$team_selector3 == "Pittsburgh Penguins") {
      scatterplot <- ggplot(Pitt_away_df, aes(x = nth_away_games, y = distance.from.arena.in.miles, color = W.or.L, text = W.or.L)) +
        geom_point() +
        scale_color_manual(values = c("Pitt W" = "#FFB81C", "Pitt L" = "#CC9612")) +
        labs(title = "Travel Distance and n-th Away Games")
    } else {
      scatterplot <- ggplot() +
        geom_point(data = Sea_away_df, aes(x = nth_away_games, y = distance.from.arena.in.miles, color = W.or.L, text = W.or.L)) +
        geom_point(data = Pitt_away_df, aes(x = nth_away_games, y = distance.from.arena.in.miles, color = W.or.L, text = W.or.L)) +
        scale_color_manual(values = c("Sea W " = "#85c1ff", "Sea L" = "#416481", "Pitt W" = "#FFB81C", "Pitt L" = "#CC9612")) +
        labs(title = "Travel Distance and n-th Away Games")
    }
    
    return(ggplotly(scatterplot, tooltip = "text"))
    
  })
}