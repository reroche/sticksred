library(shiny)
library(tidyverse)
library(highcharter)
library(metricsgraphics)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(radarchart)
source("utils.R")

shinyServer(function(input, output) {
  # Player Attribute Radar Chart
  player_attr_names <- rownames(radar_df)
  data.player1 <- reactive({switch(input$player1,
                                   "Andres Iniesta"=select(radar_df, Label, `Andres Iniesta`),
                                   "Arjen Robben"=select(radar_df, Label, `Arjen Robben`),
                                   "Cristiano Ronaldo"=select(radar_df, Label, `Cristiano Ronaldo`),
                                   "Eden Hazard"=select(radar_df, Label, `Eden Hazard`),
                                   "Lionel Messi"=select(radar_df, Label, `Lionel Messi`),
                                   "Luis Suarez"=select(radar_df, Label, `Luis Suarez`),
                                   "Manuel Neuer"=select(radar_df, Label, `Manuel Neuer`),
                                   "Mesut Ozil"=select(radar_df, Label, `Mesut Oezil`),
                                   "Neymar"=select(radar_df, Label, `Neymar`),
                                   "Zlatan Ibrahimovic"=select(radar_df, Label, `Zlatan Ibrahimovic`)
                                   )})
  output$plot1 <- renderChartJSRadar({
    dat <- data.player1()
    chartJSRadar(scores=dat, maxScale=100, showToolTipLabel=TRUE,
                 labelSize=10)
  })
  # Density bar chart of player attributes
  output$plot2 <- renderHighchart({
    data.temp <- reactive({switch(input$var, 
                                  "Dribbling"=select(player_stats_elias, dribbling),
                                  "Reactions"=select(player_stats_elias, reactions),
                                  "Finishing"=select(player_stats_elias, finishing),
                                  "Stamina"=select(player_stats_elias, stamina),
                                  "Overall Rating"=select(player_stats_elias, overall_rating))})
                                  
    dat <- data.temp() %>%
      table() %>%
      as.data.frame()
    hchart(dat, "column", hcaes(x=., y=Freq)) %>%
      hc_title(text=paste("Distribtuion of ", input$var))
  })
  
  data.temp2 <- reactive({switch(input$dataset2, "France" = france1, "Spain" = spain1,
                                "Germany" = germany1, "Italy" = italy1, "England" = england1)})
  
  output$plot3 <- renderHighchart({
    dat <- data.temp2()
    hchart(dat, type="column", hcaes(x=team_name, y=win_percentage)) %>%
      hc_title(text="Winning Percentage of Teams Across League")
  })
  # Highchart
  output$plot4 <- renderHighchart({
    checker = input$dataset2
    if (checker == "France") {dat <- france1} 
    else if (checker == "Spain") {dat <- spain1}
    else if (checker == "Germany") {dat <- germany1}
    else if (checker == "England") {dat <- england1}
    else {dat <- italy1}
    hc <- highchart() %>%
      hc_title(text = paste("How",checker,"performed")) %>% 
      hc_add_series_scatter(dat$win_percentage, dat$wins, dat$total_matches, label =
                              dat$team_name) %>%
      hc_subtitle(text = "Win Percentage, Wins, and Total Matches for Top teams") %>%
      hc_add_theme(hc_theme_google())
    hc
  })
  output$plot5 <- renderPlot({
    data.temp <- reactive({switch(input$Country, "France" = france_perc, "Spain" = spain_perc,
                                  "England" = england_perc, "Germany" = germany_perc, "Italy" = italy_perc)})
    ggplot(data.temp(),
           aes_string(x="variable", y="value", col = "team_name")) +
      geom_point() + geom_line(aes_string(group = "team_name")) +
      labs(title = ifelse(input$Country == "France",
                          "Win Percentage for Top 10 Teams in France",
                          ifelse(input$Country == "Germany",
                                 "Win Percentage for Top 10 Teams in Germany",
                                 ifelse(input$Country == "Italy",
                                        "Win Percentage for Top 10 Teams in Italy",
                                        ifelse(input$Country == "Spain",
                                               "Win Percentage for Top 9 Teams in Spain",
                                               "Win Percentage for Top 10 Teams in England")))),
           x = "Season", y = "Win Percentage", fill = "Team Name",
           caption = "Source: European Soccer Database") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot6 <- renderPlot({
    data.temp.1 <- reactive({switch(input$Country_win, "France" = france_win, "Spain" = spain_win,
                                    "England" = england_win, "Germany" = germany_win, "Italy" = italy_win)})
    ggplot(data.temp.1(),
           aes_string(x = "team_name", y = input$style, fill = "team_name")) +
      geom_histogram(stat = "identity") +
      geom_text(aes(label = round(total_win_percentage,2), y =  2), size = 2) +
      labs(title = ifelse(input$Country_win == "France",
                          "Score for Top 10 Teams in France",
                          ifelse(input$Country_win == "Germany",
                                 "Score for Top 10 Teams in Germany",
                                 ifelse(input$Country_win == "Italy",
                                        "Score for Top 10 Teams in Italy",
                                        ifelse(input$Country_win == "Spain",
                                               "Score for Top 9 Teams in Spain",
                                               "Score for Top 10 Teams in England")))),
           x = "Team", y = "Score", fill = "Team Name",
           caption = "Source: European Soccer Database") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$plot7 <- renderPlotly({
    p7 <- ggplot(players_perf, aes_string(x = "age")) +
      geom_density(adjust = input$bandwidth, aes(color = league)) +
      theme(plot.title = element_text(size=10), axis.title = element_text(size=10), 
            axis.text = element_text(size=8), legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      labs(title = "Top Players Age Distribution",
           x = "Age",
           y = "Density")
    
    ggplotly(p7) %>%
      layout(legend = list(
        orientation = "h",
        y = -0.2
      ))
  })
  output$plot8 <- renderPlotly({
    p8 <- ggplot(subset(players_season, avgRating > input$scoreRange), 
                 aes(x = season, y = avgRating,
                     color = team_long_name, label = player_name)) +
      geom_jitter() +
      theme(plot.title = element_text(size=10), axis.title = element_text(size=10), 
            axis.text = element_text(size=8), legend.text = element_text(size=8),
            legend.title = element_text(size=8)) +
      labs(title = "Top Players For Each Season",
           x = "Season",
           y = "Player Rating",
           color = "Club")
    
    ggplotly(p8) %>%
      layout(legend = list(
        orientation = "h",
        y = -0.2
      ))
  })
})