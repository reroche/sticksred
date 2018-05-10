# Import libraries
library(tidyverse)
library(RSQLite)
library(dplyr)
library(stringr)
library(plotly)
library(DT)
library(tidyr)
library(eeptools)
library(reshape2)

# Import data
sqlite_drv <- dbDriver("SQLite")
sql_db <- dbConnect(sqlite_drv,"database.sqlite")
Country <- dbGetQuery(sql_db,"Select * from Country")
Match <- dbGetQuery(sql_db,"Select * from Match")
League <- dbGetQuery(sql_db,"Select * from League")
Team <- dbGetQuery(sql_db,"Select * from Team")
team_attributes_df <- dbGetQuery(sql_db,"select * from Team_Attributes")

match_df = Match
team_df = Team
country_df = Country
league_df = League

# Elias

player <- dbGetQuery(sql_db, "Select * from Player")
player_stats <- dbGetQuery(sql_db, "Select * from Player_Attributes")

# Create player stats dataframe
player_stats_elias <-
  player_stats %>%
  rename(player_stats_id=id) %>%
  left_join(player, by="player_api_id") %>%
  group_by(player_api_id) %>%
  top_n(n=1, wt=date) %>%
  arrange(desc(overall_rating)) %>%
  as.data.frame()

radar_df <-
  head(player_stats_elias, 5) %>%
  select(player_name, 10:42) %>%
  as.data.frame() %>%
  gather(key=Label, value=Score, -player_name) %>%
  spread(key=player_name, value=Score)

# Kaushik

# Count the number of matches the particular team played at home
home_match <- count(Match,home_team_api_id)
away_match <- count(Match,away_team_api_id)

# Change names of the column n to number of matches
names(home_match)[names(home_match)=="n"] <- "home_matches_number"
names(away_match)[names(away_match)=="n"] <- "away_matches_number"

new_match_data <- cbind(home_match, away_match)

# Add three new columns:
# a. total matches a particular team has played 
# b. how many matches the team has won at home and away
# c. what is the winning percentage. (wins/total_matches * 100)
new_match_data <- new_match_data %>% mutate(
  total_matches = home_matches_number + away_matches_number,
  wins = 0,
  win_percentage = 0,
  country = "",
  team_name = ""
)

# Find the country of the team and append in the country coloumn
for(row1 in rownames(new_match_data))
{
  home_indexes = which(Match$home_team_api_id == new_match_data$home_team_api_id[as.numeric(row1)])
  new_match_data$country[as.numeric(row1)] <- Country$name[Country$id==Match$country_id[as.numeric(home_indexes[1])]]
  new_match_data$team_name[as.numeric(row1)] <- Team$team_long_name[Team$team_api_id==new_match_data$home_team_api_id[as.numeric(row1)]]
}

# Drop the unnecessary columns "home_matches_number" and "away_matches_number"
drops_columns <- c("home_matches_number","away_matches_number")
new_match_data <- new_match_data[ , !(names(new_match_data) %in% drops_columns)]

for(id in rownames(new_match_data))
{
  # win_count stores the number of wins if the current team has scored more goals than the opponent team.
  win_count = 0 
  # Find all the records in main "Match" table which match the current team id
  home_indexes = which(Match$home_team_api_id == new_match_data$home_team_api_id[as.numeric(id)])
  away_indexes = which(Match$away_team_api_id == new_match_data$away_team_api_id[as.numeric(id)])
  for(i in home_indexes)
  {
    if(Match$home_team_goal[i]>Match$away_team_goal[i])
    {
      win_count = win_count +1 
    }
  }
  for(i in away_indexes)
  {
    if(Match$away_team_goal[i]>Match$home_team_goal[i])
    {
      win_count = win_count + 1
    }
  }
  new_match_data$wins[as.numeric(id)] <- win_count
  new_match_data$win_percentage[as.numeric(id)] <- as.double(win_count/new_match_data$total_matches[as.numeric(id)]*100)
}

# Drop Away_team_id column and change home_team_api_id columns name to team_id
drop_columns <- c("away_team_api_id")
new_match_data <- new_match_data[ , !names(new_match_data) %in% drop_columns]
names(new_match_data)[names(new_match_data)=="home_team_api_id"]<-"team_id"

# Sort the teams based on the winning percentage
sorted_data <- new_match_data[order(-new_match_data$win_percentage),]

english_team <- sorted_data[sorted_data$country=="England",]
england1 <- english_team[order(-english_team$win_percentage),][1:10,]

spain_teams <- sorted_data[sorted_data$country=="Spain",]
spain1 <- spain_teams[order(-spain_teams$win_percentage),][1:10,]

german_team <- sorted_data[sorted_data$country=="Germany",]
germany1 <- german_team[order(-german_team$win_percentage),][1:10,]

france_team <- sorted_data[sorted_data$country=="France",]
france1 <- france_team[order(-france_team$win_percentage),][1:10,]

italy_team <- sorted_data[sorted_data$country=="Italy",]
italy1 <- italy_team[order(-italy_team$win_percentage),][1:10,]

teams.dat = rbind(germany1, spain1, england1, france1, italy1)

# Kabir
home_match = count(match_df, home_team_api_id)
away_match = count(match_df,away_team_api_id)
names(home_match)[names(home_match)=="n"] <- "home_matches_number"
names(away_match)[names(away_match)=="n"] <- "away_matches_number"
new_match_data = cbind(home_match,away_match)
new_match_data <- new_match_data %>% mutate(
  total_matches = home_matches_number + away_matches_number,
  `Win Percentage 08` = 0,
  `Win Percentage 09` = 0,
  `Win Percentage 10` = 0,
  `Win Percentage 11` = 0,
  `Win Percentage 12` = 0,
  `Win Percentage 13` = 0,
  `Win Percentage 14` = 0,
  `Win Percentage 15` = 0,
  home_win_percentage = 0,
  away_win_percentage = 0,
  home_advantage = 0,
  total_win_percentage = 0,
  country = "",
  team_name = ""
)

for(i in rownames(new_match_data))
{
  home_indexes = which(match_df$home_team_api_id == new_match_data$home_team_api_id[as.numeric(i)])
  new_match_data$country[as.numeric(i)] <- country_df$name[country_df$id==match_df$country_id[as.numeric(home_indexes[1])]]
  new_match_data$team_name[as.numeric(i)] <- team_df$team_long_name[team_df$team_api_id==new_match_data$home_team_api_id[as.numeric(i)]]
}
drops_columns <- c("home_matches_number","away_matches_number")
new_match_data <- new_match_data
for(team in rownames(new_match_data)) {
  win_percentages = c()
  total_wins = 0
  home_wins = 0
  away_wins = 0
  for(season in unique(match_df$season)) {
    win_count = 0
    season_matches = match_df[which(match_df$season == season), ]
    home_indexes = which(season_matches$home_team_api_id == new_match_data$home_team_api_id[as.numeric(team)])
    away_indexes = which(season_matches$away_team_api_id == new_match_data$away_team_api_id[as.numeric(team)])
    for(i in home_indexes) {
      if(season_matches$home_team_goal[i] > season_matches$away_team_goal[i]) {
        win_count = win_count + 1
        total_wins = total_wins + 1
        home_wins = home_wins + 1
      }
    }
    for(i in away_indexes) {
      if(season_matches$away_team_goal[i] > season_matches$home_team_goal[i]) {
        win_count = win_count + 1
        total_wins = total_wins + 1
        away_wins = away_wins + 1
      }
    }
    if(win_count == 0){
      percentage = NA
    }
    else{
      percentage = as.double(win_count/(length(home_indexes)+length(away_indexes))*100)
    }
    win_percentages = c(win_percentages, percentage)
  }
  new_match_data$`Win Percentage 08`[as.numeric(team)] = win_percentages[1]
  new_match_data$`Win Percentage 09`[as.numeric(team)] = win_percentages[2]
  new_match_data$`Win Percentage 10`[as.numeric(team)] = win_percentages[3]
  new_match_data$`Win Percentage 11`[as.numeric(team)] = win_percentages[4]
  new_match_data$`Win Percentage 12`[as.numeric(team)] = win_percentages[5]
  new_match_data$`Win Percentage 13`[as.numeric(team)] = win_percentages[6]
  new_match_data$`Win Percentage 14`[as.numeric(team)] = win_percentages[7]
  new_match_data$`Win Percentage 15`[as.numeric(team)] = win_percentages[8]
  new_match_data$home_win_percentage[as.numeric(team)] = as.double(home_wins/new_match_data$home_matches_number[as.numeric(team)]*100)
  new_match_data$away_win_percentage[as.numeric(team)] = as.double(away_wins/new_match_data$away_matches_number[as.numeric(team)]*100)
  new_match_data$total_win_percentage[as.numeric(team)] = as.double(total_wins/new_match_data$total_matches[as.numeric(team)]*100)
}
drop_columns <- c("away_team_api_id")
new_match_data <- na.omit(new_match_data)
new_match_data <- new_match_data
new_match_data$home_advantage = new_match_data$home_win_percentage - new_match_data$away_win_percentage
sorted_data = new_match_data[order(-new_match_data$total_win_percentage),]
german_team <- sorted_data[sorted_data$country=="Germany",]
germany <- german_team[order(-german_team$home_advantage),][1:10,]
italian_team <- sorted_data[sorted_data$country=="Italy",]
italy <- italian_team[order(-italian_team$home_advantage),][1:10,]
spanish_team <- sorted_data[sorted_data$country=="Spain",]
spain <- spanish_team[order(-spanish_team$home_advantage),][1:9,]
english_team <- sorted_data[sorted_data$country=="England",]
england <- english_team[order(-english_team$home_advantage),][1:10,]
french_team <- sorted_data[sorted_data$country=="France",]
france <- french_team[order(-french_team$home_advantage),][1:10,]
germany_win = head(german_team[order(-german_team$total_win_percentage),],10)
germany_ids = germany_win$home_team_api_id
france_win = head(french_team[order(-french_team$total_win_percentage),],10)
france_ids = france_win$home_team_api_id
italy_win = head(italian_team[order(-italian_team$total_win_percentage),],10)
italy_ids = italy_win$home_team_api_id
england_win = head(english_team[order(-english_team$total_win_percentage),],10)
england_ids = england_win$home_team_api_id
spain_win = head(spanish_team[order(-spanish_team$total_win_percentage),],10)
spain_ids = spain_win$home_team_api_id
team_scores = function(ids, option = "Offensive"){
  scores = c()
  if(option == "Offensive"){
    for(team in ids){
      speed = mean(team_attributes_df$buildUpPlaySpeed[which(team_attributes_df$team_api_id == team)])
      passing = mean(team_attributes_df$chanceCreationPassing[which(team_attributes_df$team_api_id == team)])
      crossing = mean(team_attributes_df$chanceCreationCrossing[which(team_attributes_df$team_api_id == team)])
      shooting = mean(team_attributes_df$chanceCreationShooting[which(team_attributes_df$team_api_id == team)])
      scores = c(scores, speed + passing + crossing + shooting)
    }
  }
  if(option == "Defensive"){
    for(team in ids){
      pressure = mean(team_attributes_df$defencePressure[which(team_attributes_df$team_api_id == team)])
      aggression = mean(team_attributes_df$defenceAggression[which(team_attributes_df$team_api_id == team)])
      scores = c(scores, pressure + aggression)
    }
  }
  return(scores)
}
germany_win$offensive_score = team_scores(germany_ids)
france_win$offensive_score = team_scores(france_ids)
italy_win$offensive_score = team_scores(italy_ids)
spain_win$offensive_score = team_scores(spain_ids)
england_win$offensive_score = team_scores(england_ids)
germany_win$defensive_score = team_scores(germany_ids, "Defensive")
france_win$defensive_score = team_scores(france_ids, "Defensive")
italy_win$defensive_score = team_scores(italy_ids, "Defensive")
spain_win$defensive_score = team_scores(spain_ids, "Defensive")
england_win$defensive_score = team_scores(england_ids, "Defensive")
england_perc = melt(england_win[,c(6,7,8,9,10,11,12,13,19)], id=c("team_name"))
germany_perc = melt(germany_win[,c(6,7,8,9,10,11,12,13,19)], id=c("team_name"))
spain_perc = melt(spain_win[,c(6,7,8,9,10,11,12,13,19)], id=c("team_name"))
italy_perc = melt(italy_win[,c(6,7,8,9,10,11,12,13,19)], id=c("team_name"))
france_perc = melt(france_win[,c(6,7,8,9,10,11,12,13,19)], id=c("team_name"))

# Shihan
# Plotly Data
Time = '2014-07-01'

players_perf = NULL
leagueid = c('10257', '4769', '7809', '21518', '1729')
leaguename = c('Italy', 'France', 'Germany', 'Spain', 'England')

# italy, france, germany, spain, england
for (id in leagueid) {
  ## select all the matches in the last two seasons
  sqlcmd1 = paste(sprintf("SELECT * FROM Match WHERE league_id = %s AND date >= ", id), Time, sep = "")
  Match <- tbl_df(dbGetQuery(sql_db, sqlcmd1))
  
  ## select teams' and players' id and names
  teams = tbl_df(dbGetQuery(sql_db,"SELECT team_api_id, team_long_name FROM Team"))
  players = tbl_df(dbGetQuery(sql_db,"SELECT player_api_id, player_name, birthday, height, weight FROM Player"))
  
  ## select players' ratings in the last two seasons
  sqlcmd2 = paste("SELECT player_api_id, date, overall_rating, potential FROM Player_Attributes WHERE date >= ", Time, sep = "")
  players_attr = tbl_df(dbGetQuery(sql_db, sqlcmd2))
  
  home_player <- tbl_df(dbGetQuery(sql_db, sprintf("SELECT id, date, home_team_api_id, home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, home_player_6, home_player_7, home_player_8, home_player_9, home_player_10, home_player_11 FROM Match WHERE league_id = %s AND date >= '2014-07-01'", id)))
  home_player <- gather(home_player, player_num, player_id, home_player_1:home_player_11, factor_key=F)
  colnames(home_player)[3] = "team_api_id"
  
  away_player <- tbl_df(dbGetQuery(sql_db, sprintf("SELECT id, date, away_team_api_id, away_player_1, away_player_2, away_player_3, away_player_4, away_player_5, away_player_6, away_player_7, away_player_8, away_player_9, away_player_10, away_player_11 FROM Match WHERE league_id = %s AND date >= '2014-07-01'", id)))
  away_player <- gather(away_player, player_num, player_id, away_player_1:away_player_11, factor_key=F)
  colnames(away_player)[3] = "team_api_id"
  
  players_rating = players_attr %>% 
    group_by(player_api_id) %>% summarise(nRating = n(), avgRating = mean(overall_rating), avgPotential = mean(potential))
  
  players_performance = rbind(home_player, away_player) %>% 
    group_by(team_api_id, player_id) %>% summarise(nGames = n()) %>%
    left_join(teams, by = "team_api_id") %>% 
    left_join(players, by = c("player_id" = "player_api_id")) %>% 
    left_join(players_rating, by = c("player_id" = "player_api_id")) %>% 
    filter(!is.na(player_name)) %>%
    mutate(birthday = sapply(birthday, str_sub, 1, 10), avgRating = sapply(avgRating, round, 3), avgPotential = sapply(avgPotential, round, 3)) %>%
    filter(!is.na(avgRating))
  
  players_performance = subset(players_performance, select = c(team_long_name,player_name,birthday,height,weight,avgRating,avgPotential))
  
  players_performance <- players_performance[order(players_performance$avgRating, decreasing = TRUE),] 
  players_performance <- head(players_performance, 50)
  players_performance$league <- rep(leaguename[match(id, leagueid)],
                                    nrow(players_performance))
  
  if (is.null(players_perf)) {
    players_perf = players_performance
  } else {
    players_perf <- rbind(players_perf, players_performance)
  }
}

curDate <- as.Date("2018-01-01")
players_perf$age <- round(age_calc(as.Date(players_perf$birthday), 
                                   curDate, units = "years"))

# Another Plotly
Times = c('2012-08-01', '2013-08-01', '2014-08-01', '2015-08-01', '2016-08-01')
idxes  = c(1,2,3,4)

players_season = NULL

for (idx in idxes) {
  sqlcmd12 = paste(sprintf("SELECT * FROM Match WHERE date >= '%s' AND date <= '%s'", Times[idx], Times[idx+1]), sep = "")
  Match2 <- tbl_df(dbGetQuery(sql_db, sqlcmd12))
  
  ## select teams' and players' id and names
  teams2 = tbl_df(dbGetQuery(sql_db,"SELECT team_api_id, team_long_name FROM Team"))
  players2 = tbl_df(dbGetQuery(sql_db,"SELECT player_api_id, player_name, birthday, height, weight FROM Player"))
  
  ## select players' ratings in the last two seasons
  sqlcmd22 = paste(sprintf("SELECT player_api_id, date, overall_rating, potential FROM Player_Attributes WHERE date >= '%s' AND date <= '%s'", Times[idx], Times[idx+1]), sep = "")
  players_attr2 = tbl_df(dbGetQuery(sql_db, sqlcmd22))
  
  home_player2 <- tbl_df(dbGetQuery(sql_db, sprintf("SELECT id, date, home_team_api_id, home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, home_player_6, home_player_7, home_player_8, home_player_9, home_player_10, home_player_11 FROM Match WHERE date >= '%s' AND date <= '%s'", Times[idx], Times[idx+1])))
  home_player2 <- gather(home_player2, player_num, player_id, home_player_1:home_player_11, factor_key=F)
  colnames(home_player2)[3] = "team_api_id"
  
  away_player2 <- tbl_df(dbGetQuery(sql_db, sprintf("SELECT id, date, away_team_api_id, away_player_1, away_player_2, away_player_3, away_player_4, away_player_5, away_player_6, away_player_7, away_player_8, away_player_9, away_player_10, away_player_11 FROM Match WHERE date >= '%s' AND date <= '%s'", Times[idx], Times[idx+1])))
  away_player2 <- gather(away_player2, player_num, player_id, away_player_1:away_player_11, factor_key=F)
  colnames(away_player2)[3] = "team_api_id"
  
  players_rating2 = players_attr %>% 
    group_by(player_api_id) %>% summarise(nRating = n(), avgRating = mean(overall_rating), avgPotential = mean(potential))
  
  players_performance2 = rbind(home_player2, away_player2) %>% 
    group_by(team_api_id, player_id) %>% summarise(nGames = n()) %>%
    left_join(teams, by = "team_api_id") %>% 
    left_join(players, by = c("player_id" = "player_api_id")) %>% 
    left_join(players_rating, by = c("player_id" = "player_api_id")) %>% 
    filter(!is.na(player_name)) %>%
    mutate(birthday = sapply(birthday, str_sub, 1, 10), avgRating = sapply(avgRating, round, 3), avgPotential = sapply(avgPotential, round, 3)) %>%
    filter(!is.na(avgRating))
  
  players_performance2 = subset(players_performance2, select = c(team_long_name,player_name,avgRating))
  
  players_performance2 <- players_performance2[order(players_performance2$avgRating, decreasing = TRUE),] 
  players_performance2 <- head(players_performance2, 20)
  players_performance2$season <- rep(substr(Times[idx+1], 1, 4),
                                     nrow(players_performance2))
  
  if (is.null(players_season)) {
    players_season = players_performance2
  } else {
    players_season <- rbind(players_season, players_performance2)
  }
}