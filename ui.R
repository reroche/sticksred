# Import libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(highcharter)
library(radarchart)
library(metricsgraphics)
library(htmlwidgets)
library(htmltools)

dashboardPage(skin="red",
  dashboardHeader(title="Sticks Red"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName="intro"),
      menuItem("Players", tabName="tab1"),
      menuItem("Teams", tabName="tab2"),
      menuItem("Home Advantage", tabName="tab3"),
      menuItem("Age", tabName="tab4")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="intro",
        column(width=12,
          box(
            h1("Title"),
            p("Welcome to our Shiny Web App!"),
            p("We have two paragraphs.")
          )
        )
      ),
      tabItem(tabName="tab1",
        fluidRow(
          box(chartJSRadarOutput("plot1", height=500))
        ),
        fluidRow(
          box(highchartOutput("plot2", height=300)),
          box(
            title="Controls",
            selectInput("var", "Player Attribute", c("Dribbling",
                                                     "Reactions",
                                                     "Finishing",
                                                     "Stamina",
                                                     "Overall Rating"))
          )
        )
      ),
      tabItem(tabName="tab2",
        fluidRow(
          box(highchartOutput("plot3", height=300)),
          box(
            title="Controls",
            selectInput("dataset2", "Country", c("France", "Spain", "Germany", "Italy", "England")),
            radioButtons("which_variable", label = "Which variable?",
                   choices=c("Wins"="wins",
                               "Win Percentage"="win_percentage"))
          )
        ),
        fluidRow(
          box(highchartOutput("plot4"))
        )
      ),
      # tabItem(tabName="tab3",
      #   fluidRow(
      #     box(plotOutput("plot5", height=300)),
      #     box(
      #       inputPanel(
      #         selectInput("Country", "Dataset", c("France", "Spain", "England", "Germany", "Italy"), selected = "England")
      #       )
      #     )
      #   ),
      #   fluidRow(
      #     box(plotOutput("plot6", height=300)),
      #     box(
      #       inputPanel(
      #         selectInput("Country", "Dataset", c("France", "Spain", "England", "Germany", "Italy"), selected = "England")
      #       ),
      #       radioButtons("style", label = "Which Style?",
      #                    choices = c("Offensive" = "offensive_score",
      #                                "Defensive" = "defensive_score"))
      #     )
      #   )
      # ),
      tabItem(tabName="tab3",
              fluidRow(
                box(plotOutput("plot5", height=300)),
                box(
                  inputPanel(
                    selectInput("Country", "League", c("France", "Spain", "England", "Germany", "Italy"), selected = "England")
                  ),
                  p("This trend plot displays the winning percetages for each of the top 10 teams in 
              the top 5 leagues in Europe over the course of 8 seasons. Notice how the there are some teams
              in leagues that always have consistently higher winning percentages that the rest of
              the league. This is a good indicator of how competitive the league is. In England, most
              of the teams are close to together, demonstrating that there is not single dominent team
              in the league. To constrast, in Spain, Barcelona and Real Madrid are almost always the teams
              with the highest winning percentage each season, showing that they are dominant in their league.")
                )
              ),
              fluidRow(
                box(plotOutput("plot6", height=300)),
                box(
                  inputPanel(
                    selectInput("Country_win", "League", c("France", "Spain", "England", "Germany", "Italy"), selected = "England")
                  ),
                  radioButtons("style", label = "Which Style?",
                               choices = c("Offensive" = "offensive_score",
                                           "Defensive" = "defensive_score")),
                  p("This barplot displays the Offensive and Defensive scores of each of the top 10 teams in the top 5 leagues
              across Europe. The scores were calculated through using the fifa scores the describe each teams playing 
              style. The offensive score was calculated using the following equation: mean(buildUpPlaySpeed) + 
              mean(chanceCreationPassing) + mean(chanceCreationCrossing) + mean(chanceCreationShooting). Mean was used
              to average each score from the 7 fifa games data given. The defensive score was calculated using the
              following equation: mean(defencePressure) + mean(defenceAggression). There are fewer terms involved there
              were not as many relevant variables for defense as there were for offense, however, these variables are
              quite telling. Notice how Offensive score is not necessarily correlated with winning percentage, and
              that a team's defensive score plays more of a role.")
                )
              )
      ),
      tabItem(tabName="tab4",
        fluidRow(
          box(plotlyOutput("plot7", height=500, width=500))
        ),
        fluidRow(
          box(plotlyOutput("plot8", height=500, width=500))
        )
      ) 
    )
  )
)