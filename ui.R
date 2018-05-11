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
      menuItem("Team Performance", tabName="tab2"),
      menuItem("Competitiveness", tabName="tab3"),
      menuItem("Age and Rating", tabName="tab4"),
      menuItem("Players", tabName="tab1")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="intro",
        column(width=12,
          box(
            h1("Introduction"),
            p("Soccer is the most popular sport in the world, a sport that reaches out to children and adults alike and something that brings people together. In light of the upcoming FIFA World Cup in Russia, pundits have been discussing how the competition between clubs in the European leagues translates into international success. Do countries succeed in the World Cup due to team chemistry and strong dynamics or as a result of superstars returning home from club success abroad? We look into how different teams from different European leagues match up domestically and internationally and hope to shed some light on the age old question, which really is the most competitive league in Europe?"),
            p("To do so, we looked into the European Soccer Database, a SQLite database, from Kaggle. This dataset offers match and player data about over 25,000 matches and 10,000 games from 2008 to 2016. It also references player and team ratings obtained from EA Sports’ FIFA video game series allowing us to integrate team records with team ratings for a holistic view into a team’s performance.")
          )
        )
      ),
      tabItem(tabName="tab1",
        fluidRow(
          box(chartJSRadarOutput("plot1", height=500)),
          box(
            title="Controls",
            selectInput("player1", "Player", c("Andres Iniesta",
                                               "Arjen Robben",
                                               "Cristiano Ronaldo",
                                               "Eden Hazard",
                                               "Lionel Messi",
                                               "Luis Suarez",
                                               "Manuel Neuer",
                                               "Mesut Ozil",
                                               "Neymar",
                                               "Zlatan Ibrahimovic")),
          p("This radar chart shows a player's rankings across a variety of skills and attributes. The top 10 players in Europe are included as options. The rankings are in the range [0,100], and are released by FIFA every year.")
          )
        ),
        fluidRow(
          box(highchartOutput("plot2", height=300)),
          box(
            title="Controls",
            selectInput("var", "Player Attribute", c("Dribbling",
                                                     "Reactions",
                                                     "Finishing",
                                                     "Stamina",
                                                     "Overall Rating")),
            p("This graph shows the distribution of rankings for a particular skill across all players in Europe. The x-axis represents the scores for the attribute, and the y-axis represents the frequency of that score. When used with the previous graph, observations about how the skills that make a player effective can be made. For instance, we can see that Cristiano Ronaldo excels at finishing, shot power, and dribbling, which makes him an effective forward.In the distribution plot, we can see that he lies in the upper tail of these attributes. It is worth noting that some attributes, such as finishing, have an unexpected mode at 25. This is a result of estimating ratings for players (such as defenders or goalkeepers) who had insufficient display of an attribute, so 25 is commonly used instead.")
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
                               "Win Percentage"="win_percentage")),
            p("The bar chart shows a side-by-side comparison between Wins and Win Percentage for the top teams in the selected league. The control panel allows us to switch between European Leagues and switch between Wins and Win percentage. The option to toggle between Wins and Win Percentage only applies for the bar chart, as the scatter plot already shows this information on the x and y axis respectively.")
          )
        ),
        fluidRow(
          box(highchartOutput("plot4")),
          box(
            p("This scatter plot allows us to clearly see the distance between points when accounting for Wins and Win Percentage. The Y-axis corresponds to their number of Wins from 2008 - 2016 and the X-axis corresponds to their Win Percentage. The size component, when we hover over each team name, shows the number of games they have played. Looking at the distance between points, we can see how the points are clustered, allowing us to judge competitiveness to an extent.")
          )
        )
      ),
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
            we can tell that the top players from the Spanish league are on average youngest
            compared to players from other leagues, and the top players from the Italian league 
            are on average the oldest. The age distribution of top players from the German league
            has two modes, one at around 29 and another one at around 33. In general, we can tell 
            that players normally reach their peak at the age of 30 to 32. At the same time, the
            Spanish league has their top players at a relatively younger age compared to other leagues, 
            which implies that the Spanish league potentially has a better performance as it has 
            more fresh blood in the system.")
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