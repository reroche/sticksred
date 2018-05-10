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
  output$plot1 <- renderChartJSRadar({
    chartJSRadar(scores=radar_df, maxScale=100, showToolTipLabel=TRUE,
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
    hchart(dat, "column", hcaes(x=., y=Freq))
    # ggplot(dat, aes(x=., y=Freq)) +
    #   geom_bar(stat="identity", fill="darkred", color="white") +
    #   labs(y="Density", title="Distribution of Attribute among Players") +
    #   theme(axis.text.x=element_blank())
  })
  
  data.temp2 <- reactive({switch(input$dataset2, "France" = france1, "Spain" = spain1,
                                "Germany" = germany1, "Italy" = italy1, "England" = england1)})
  
  output$plot3 <- renderHighchart({
      dat <- data.temp2()
      print(names(dat))
      hchart(dat, type="column", hcaes(x=team_name, y=win_percentage))
        # geom_histogram(stat = "identity") +
        # labs(title = ifelse(input$which_variable == "wins",
        #                     "Total Wins over 9 seasons",
        #                     "Winning Percentage over 9 seasons"),
        #      x = "Team Names", 
        #      y = ifelse(input$which_variable == "wins", "Number of Wins", 
        #                 "Winning Percentage"),
        #      fill = "Team Names",
        #      caption = "Source: European Football Database") +
        # theme(axis.text.x=element_text(angle=45, hjust=1))
  })
  # Highchart
  output$plot4 <- renderHighchart({
    checker = input$dataset2
    if (checker == "France") {dat <- france1} 
    else if (checker == "Spain") {dat <- spain1}
    else if (checker == "Germany") {dat <- germany1}
    else if (checker == "England") {dat <- england1}
    else {dat <- italy1}
    # print(names(dat))
    hc <- hchart(dat, type="scatter", hcaes(x=wins, y=win_percentage))
      # hc_title(text = paste("How",checker,"performed")) 
      # hc_add_series_scatter(dat$win_percentage, dat$wins, dat$total_matches, label =
      #                        dat$team_name)
    hc
  })
  
  # output$plot4 <- renderMetricsgraphics({
  #   checker = input$dataset2
  #   if (checker == "France") {dat <- france1}
  #   else if (checker == "Spain") {dat <- spain1}
  #   else if (checker == "Germany") {dat <- germany1}
  #   else if (checker == "England") {dat <- england1}
  #   else {dat <- italy1}
  #   mjs_plot(dat, x=win_percentage, y=wins) %>%
  #     mjs_point(least_squares=TRUE, size_accessor=total_matches,
  #               color_accessor=total_matches, color_type = "category",
  #               x_rug=TRUE, y_rug=TRUE)
  # })
  # output$plot5 <- renderPlot({
  #   data.temp <- reactive({switch(input$Country, "France" = france, "Spain" = spain,
  #                                 "England" = england, "Germany" = germany, "Italy" = italy)})
  #   ggplot(data.temp(),
  #          aes_string(x = "team_name", y = "home_advantage", fill = "team_name")) +
  #     geom_histogram(bins = input$n_breaks, stat = "identity") +
  #     geom_text(aes(label = round(total_win_percentage,2), y = home_advantage + 1), size = 2) +
  #     labs(title = ifelse(input$Country == "France",
  #                         "Home Advantage for Top 10 Teams in France",
  #                         ifelse(input$Country == "Germany",
  #                                "Home Advantage for Top 10 Teams in Germany",
  #                                ifelse(input$Country == "Italy",
  #                                       "Home Advantage for Top 10 Teams in Italy",
  #                                       ifelse(input$Country == "Spain",
  #                                              "Home Advantage for Top 9 Teams in Spain",
  #                                              "Home Advantage for Top 10 Teams in England")))),
  #          x = "Team", y = "Home Advantage", fill = "Team Name",
  #          caption = "Source: European Soccer Database") + 
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
  # output$plot6 <- renderPlot({
  #   data.temp <- reactive({switch(input$Country, "France" = france_win, "Spain" = spain_win,
  #                                 "England" = england_win, "Germany" = germany_win, "Italy" = italy_win)})
  #   ggplot(data.temp(),
  #          aes_string(x = "team_name", y = input$style, fill = "team_name")) +
  #     geom_histogram(bins = input$n_breaks, stat = "identity") +
  #     labs(title = ifelse(input$Country == "France",
  #                         "Score for Top 10 Teams in France",
  #                         ifelse(input$Country == "Germany",
  #                                "Score for Top 10 Teams in Germany",
  #                                ifelse(input$Country == "Italy",
  #                                       "Score for Top 10 Teams in Italy",
  #                                       ifelse(input$Country == "Spain",
  #                                              "Score for Top 9 Teams in Spain",
  #                                              "Score for Top 10 Teams in England")))),
  #          x = "Team", y = "Score", fill = "Team Name",
  #          caption = "Source: European Soccer Database") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })
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
    p7 <- ggplot(players_perf, aes(x = age, color = league)) +
      geom_density(adjust = 1.5) + 
      labs(title = "Age Distribution Of Top Players For Each League",
           x = "Age",
           y = "Density",
           color = "League")
    ggplotly(p7)
  })
  output$plot8 <- renderPlotly({
    p8 <- ggplot(players_season, aes(x = season, y = avgRating,
                                     color = team_long_name)) +
      geom_jitter(width = 0.2) +
      theme(axis.text = element_text(angle=45, hjust=1)) +
      labs(title = "Top Players For Each Season",
           x = "Season",
           y = "Player Rating",
           color = "Club")
    ggplotly(p8)
  })
})