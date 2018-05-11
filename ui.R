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
          box(
            column(10, plotlyOutput("plot7"))
          ),
          box(inputPanel(
            sliderInput("bandwidth", label = "Adjust Band Width",
                        min = 0, max = 2.0, value = 1.0, step = 0.1)
          ), 
          p("This plot shows the age distribution of the top 50 players from each league. 
            We can smooth the curves by adjusting the bandwidth to 1.5. From the adjusted graph, 
            jwe can tell that the top players from the Spanish league are on average youngest
            jcompared to players from other leagues, and the top players from the Italian league 
            jare on average the oldest. The age distribution of top players from the German league
            jhas two modes, one at around 29 and another one at around 33. In general, we can tell 
            jthat players normally reach their peak at the age of 30 to 32. At the same time, the
            jSpanish league has their top players at a relatively younger age compared to other leagues, 
            jwhich implies that the Spanish league potentially has a better performance as it has 
            jmore fresh blood in the system.")
          )
        ),
        
        fluidRow(
          box(
            column(10, plotlyOutput("plot8"))
          ), 
          box(inputPanel(
            sliderInput("scoreRange", label = "Score Range",
                        min = 80, max = 100, value = 89, step = 1)
          ),
          p("This plot shows the top players ratings of each season across all the leagues.
            Each point in the graph represents a player with their overall rating and the
            season based on which that rating is caculated. The colors on the points represent
            different clubs, giving us a way to relate the player's performance with the club's
            performance. As clearly shown in the graph, the top two players each eason from season 2012 to 
            season 2016 are always from Barcelona and Real Madrid, implying the dominant performance of 
            these two clubs among all other clubs.")
          )
        ) 
      )
    )
  )
)