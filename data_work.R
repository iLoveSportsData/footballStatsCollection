#----------------------------------------------------------------------------------------------#
#                                                                                              #
#                       create a football betting app - data work                              #
#                                                                                              #
#----------------------------------------------------------------------------------------------#

##  libraries  ##
library(openxlsx)
library(XLConnect)
library(tidyverse)

#------------------------------------------------------------------------#
#                             data import                                #
#------------------------------------------------------------------------#

tmp <- tempfile(fileext = ".xlsx")
download.file(url = "http://football-data.co.uk/mmz4281/1819/all-euro-data-2018-2019.xlsx", destfile = tmp, mode = "wb")

sheets <- getSheetNames(tmp)
SheetList <- lapply(sheets, read.xlsx, xlsxFile = tmp)
names(SheetList) <- sheets
data <- do.call("bind_rows", SheetList)

##  für Referee-Daten, nicht nur aktuelles Jahr, sondern auch Vorjahr!

tmp1 <- tempfile(fileext = ".xlsx")
download.file(url = "http://football-data.co.uk/mmz4281/1718/all-euro-data-2017-2018.xlsx", destfile = tmp1, mode = "wb")

sheets1 <- getSheetNames(tmp1)
SheetList1 <- lapply(sheets1, read.xlsx, xlsxFile = tmp1)
names(SheetList1) <- sheets1
data_refs <- do.call("bind_rows", SheetList1)

data_refs <- bind_rows(data, data_refs)

#------------------------------------------------------------------------#
#                        data transformation teams                       #
#------------------------------------------------------------------------#

##  add columns for total cards, goals, corners, cards, shots, btts

data <- data %>%
  mutate(total_HTgoals = rowSums(data[, c("HTHG", "HTAG")])) %>% 
  mutate(total_FTgoals = rowSums(data[, c("FTHG", "FTAG")])) %>% 
  mutate(total_shots = rowSums(data[, c("HS", "AS")])) %>% 
  mutate(total_shots_ontarget = rowSums(data[, c("HST", "AST")])) %>% 
  mutate(total_corners = rowSums(data[, c("HC", "AC")])) %>%
  mutate(total_fouls = rowSums(data[, c("HF", "AF")])) %>% 
  mutate(total_yellow = rowSums(data[, c("HY", "AY")])) %>%
  mutate(total_red = rowSums(data[, c("HR", "AR")])) %>% 
  mutate(total_cards = rowSums(data[, c("HR", "AR", "HY", "AY")])) %>% 
  mutate(btts = ifelse(FTHG > 0 & FTAG > 0, 1, 0))
  
##  split data into home and away teams  ##
home_teams <- data[, c("Div", "HomeTeam", "FTHG", "HTHG", "Referee", "HS", "HST", "HC", "HF", "HY", "HR", 
                       "total_HTgoals", "total_FTgoals", "total_shots", "total_shots_ontarget", "total_corners",
                       "total_fouls", "total_yellow", "total_red", "total_cards", "btts")]
away_teams <- data[, c("Div", "AwayTeam", "FTAG", "HTAG", "Referee", "AS", "AST", "AC", "AF", "AY", "AR", 
                       "total_HTgoals", "total_FTgoals", "total_shots", "total_shots_ontarget", "total_corners",
                       "total_fouls", "total_yellow", "total_red", "total_cards", "btts")] 

##  add text "home" or "away" in order to make statistics related to that
home_teams$place <- paste0("home")
away_teams$place <- paste0("away")

##  combine data  ##
names(away_teams) <- names(home_teams)
data_full <- bind_rows(home_teams, away_teams)

##  change league names  ##
data_full$Div <- str_replace(data_full$Div, "E0", "England - Premier League")
data_full$Div <- str_replace(data_full$Div, "E1", "England - Championship")
data_full$Div <- str_replace(data_full$Div, "E2", "England - League One")
data_full$Div <- str_replace(data_full$Div, "E3", "England - League Two")
data_full$Div <- str_replace(data_full$Div, "EC", "England - National League")
data_full$Div <- str_replace(data_full$Div, "D1", "Germany - Bundesliga")
data_full$Div <- str_replace(data_full$Div, "D2", "Germany - 2. Bundesliga")
data_full$Div <- str_replace(data_full$Div, "SC0", "Scotlang - Premiership")
data_full$Div <- str_replace(data_full$Div, "SC1", "Scotland - Championship")
data_full$Div <- str_replace(data_full$Div, "SC2", "Scotland - League One")
data_full$Div <- str_replace(data_full$Div, "SC3", "Scotland - League Two")
data_full$Div <- str_replace(data_full$Div, "SP1", "Spain - La Liga")
data_full$Div <- str_replace(data_full$Div, "SP2", "Spain - La Liga 2")
data_full$Div <- str_replace(data_full$Div, "I1", "Italy - Serie A")
data_full$Div <- str_replace(data_full$Div, "I2", "Italy - Serie B")
data_full$Div <- str_replace(data_full$Div, "F1", "France - Ligue 1")
data_full$Div <- str_replace(data_full$Div, "F2", "France - Ligue 2")
data_full$Div <- str_replace(data_full$Div, "N1", "Netherlands - Eredivisie")
data_full$Div <- str_replace(data_full$Div, "B1", "Belgium - Jupiler League")
data_full$Div <- str_replace(data_full$Div, "P1", "Portugal - Primeira Liga")
data_full$Div <- str_replace(data_full$Div, "T1", "Turkey - Super Lig")
data_full$Div <- str_replace(data_full$Div, "G1", "Greece - Super League")

data_full$Div <- as.factor(data_full$Div)

#------------------------------------------------------------------------#
#                        data transformation refs                        #
#------------------------------------------------------------------------#

##  add columns for total cards, goals, corners, cards, shots, btts

data_full_ref <- data_refs %>%
  mutate(total_yellow = rowSums(data_refs[, c("HY", "AY")])) %>%
  mutate(total_red = rowSums(data_refs[, c("HR", "AR")])) %>% 
  mutate(total_cards = rowSums(data_refs[, c("HR", "AR", "HY", "AY")]))

##  change league names  ##
data_full_ref$Div <- str_replace(data_full_ref$Div, "E0", "England - Premier League")
data_full_ref$Div <- str_replace(data_full_ref$Div, "E1", "England - Championship")
data_full_ref$Div <- str_replace(data_full_ref$Div, "E2", "England - League One")
data_full_ref$Div <- str_replace(data_full_ref$Div, "E3", "England - League Two")
data_full_ref$Div <- str_replace(data_full_ref$Div, "EC", "England - National League")
data_full_ref$Div <- str_replace(data_full_ref$Div, "D1", "Germany - Bundesliga")
data_full_ref$Div <- str_replace(data_full_ref$Div, "D2", "Germany - 2. Bundesliga")
data_full_ref$Div <- str_replace(data_full_ref$Div, "SC0", "Scotlang - Premiership")
data_full_ref$Div <- str_replace(data_full_ref$Div, "SC1", "Scotland - Championship")
data_full_ref$Div <- str_replace(data_full_ref$Div, "SC2", "Scotland - League One")
data_full_ref$Div <- str_replace(data_full_ref$Div, "SC3", "Scotland - League Two")
data_full_ref$Div <- str_replace(data_full_ref$Div, "SP1", "Spain - La Liga")
data_full_ref$Div <- str_replace(data_full_ref$Div, "SP2", "Spain - La Liga 2")
data_full_ref$Div <- str_replace(data_full_ref$Div, "I1", "Italy - Serie A")
data_full_ref$Div <- str_replace(data_full_ref$Div, "I2", "Italy - Serie B")
data_full_ref$Div <- str_replace(data_full_ref$Div, "F1", "France - Ligue 1")
data_full_ref$Div <- str_replace(data_full_ref$Div, "F2", "France - Ligue 2")
data_full_ref$Div <- str_replace(data_full_ref$Div, "N1", "Netherlands - Eredivisie")
data_full_ref$Div <- str_replace(data_full_ref$Div, "B1", "Belgium - Jupiler League")
data_full_ref$Div <- str_replace(data_full_ref$Div, "P1", "Portugal - Primeira Liga")
data_full_ref$Div <- str_replace(data_full_ref$Div, "T1", "Turkey - Super Lig")
data_full_ref$Div <- str_replace(data_full_ref$Div, "G1", "Greece - Super League")

data_full_ref$Div <- as.factor(data_full_ref$Div)

#------------------------------------------------------------------------#
#                        calculations for bets                           #
#------------------------------------------------------------------------#

#---------------------------------------------------------#
#                         goals                           #
#---------------------------------------------------------#

data_goals <- data_full %>% 
  group_by(Div, HomeTeam) %>% 
  summarize(no_games_played = length(btts),
            share_btts = sum(btts) / length(btts),
            goals_per_game = sum(total_FTgoals) / length(btts),
            teamgoals_per_game = sum(FTHG) / length(btts),
            firsthalfgoals_per_game = sum(total_HTgoals) / length(btts),
            firsthalfteamgoals_per_game = sum(HTHG) / length(btts),
            secondhalfgoals_per_game = sum(total_FTgoals - total_HTgoals) / length(btts),
            secondhalfteamgoals_per_game = sum(FTHG - HTHG) / length(btts),
            share_over0.5HT_total_goals = sum(total_HTgoals > 0) / length(btts),
            share_over1.5HT_total_goals = sum(total_HTgoals > 1) / length(btts),
            share_over2.5HT_total_goals = sum(total_HTgoals > 2) / length(btts),
            share_over0.5FT_total_goals = sum(total_FTgoals > 0) / length(btts),
            share_over1.5FT_total_goals = sum(total_FTgoals > 1) / length(btts),
            share_over2.5FT_total_goals = sum(total_FTgoals > 2) / length(btts),
            share_over3.5FT_total_goals = sum(total_FTgoals > 3) / length(btts),
            share_over0.5secondhalf_total_goals = sum(total_FTgoals - total_HTgoals > 0) / length(btts),
            share_over1.5secondhalf_total_goals = sum(total_FTgoals - total_HTgoals > 1) / length(btts),
            share_over2.5secondhalf_total_goals = sum(total_FTgoals - total_HTgoals > 2) / length(btts),
            share_over0.5FT_team_goals = sum(FTHG > 0) / length(btts),
            share_over1.5FT_team_goals = sum(FTHG > 1) / length(btts),
            share_over2.5FT_team_goals = sum(FTHG > 2) / length(btts),
            share_over0.5HT_team_goals = sum(HTHG > 0) / length(btts),
            share_over1.5HT_team_goals = sum(HTHG > 1) / length(btts),
            share_over2.5HT_team_goals = sum(HTHG > 2) / length(btts),
            share_over0.5secondhalf_team_goals = sum(FTHG - HTHG > 0) / length(btts),
            share_over1.5secondhalf_team_goals = sum(FTHG - HTHG > 1) / length(btts),
            share_over2.5secondhalf_team_goals = sum(FTHG - HTHG > 2) / length(btts))

data_goals_place <- data_full %>% 
  group_by(Div, HomeTeam, place) %>% 
    summarize(no_games_played = length(btts),
              share_btts_place = sum(btts) / length(btts),
              goals_per_game_place = sum(total_FTgoals) / length(btts),
              teamgoals_per_game_place = sum(FTHG) / length(btts),
              firsthalfgoals_per_game_place = sum(total_HTgoals) / length(btts),
              firsthalfteamgoals_per_game_place = sum(HTHG) / length(btts),
              secondhalfgoals_per_game_place = sum(total_FTgoals - total_HTgoals) / length(btts),
              secondhalfteamgoals_per_game_place = sum(FTHG - HTHG) / length(btts),
              share_over0.5HT_total_goals_place = sum(total_HTgoals > 0) / length(btts),
              share_over1.5HT_total_goals_place = sum(total_HTgoals > 1) / length(btts),
              share_over2.5HT_total_goals_place = sum(total_HTgoals > 2) / length(btts),
              share_over0.5FT_total_goals_place = sum(total_FTgoals > 0) / length(btts),
              share_over1.5FT_total_goals_place = sum(total_FTgoals > 1) / length(btts),
              share_over2.5FT_total_goals_place = sum(total_FTgoals > 2) / length(btts),
              share_over3.5FT_total_goals_place = sum(total_FTgoals > 3) / length(btts),
              share_over0.5secondhalf_total_goals_place = sum(total_FTgoals - total_HTgoals > 0) / length(btts),
              share_over1.5secondhalf_total_goals_place = sum(total_FTgoals - total_HTgoals > 1) / length(btts),
              share_over2.5secondhalf_total_goals_place = sum(total_FTgoals - total_HTgoals > 2) / length(btts),
              share_over0.5FT_team_goals_place = sum(FTHG > 0) / length(btts),
              share_over1.5FT_team_goals_place = sum(FTHG > 1) / length(btts),
              share_over2.5FT_team_goals_place = sum(FTHG > 2) / length(btts),
              share_over0.5HT_team_goals_place = sum(HTHG > 0) / length(btts),
              share_over1.5HT_team_goals_place = sum(HTHG > 1) / length(btts),
              share_over2.5HT_team_goals_place = sum(HTHG > 2) / length(btts),
              share_over0.5secondhalf_team_goals_place = sum(FTHG - HTHG > 0) / length(btts),
              share_over1.5secondhalf_team_goals_place = sum(FTHG - HTHG > 1) / length(btts),
              share_over2.5secondhalf_team_goals_place = sum(FTHG - HTHG > 2) / length(btts))

##  add column place to first data set  ##
data_goals$place <- paste0("all")

## change position in order to have both datasets equal
data_goals_place <- data_goals_place[, c("Div", "HomeTeam", "no_games_played", "share_btts_place", "goals_per_game_place", 
                                         "teamgoals_per_game_place", "firsthalfgoals_per_game_place", 
                                         "firsthalfteamgoals_per_game_place", "secondhalfgoals_per_game_place",
                                         "secondhalfteamgoals_per_game_place", "share_over0.5HT_total_goals_place", 
                                         "share_over1.5HT_total_goals_place", "share_over2.5HT_total_goals_place", 
                                         "share_over0.5FT_total_goals_place", "share_over1.5FT_total_goals_place",
                                         "share_over2.5FT_total_goals_place", "share_over3.5FT_total_goals_place", 
                                         "share_over0.5secondhalf_total_goals_place", "share_over1.5secondhalf_total_goals_place",
                                         "share_over2.5secondhalf_total_goals_place", "share_over0.5FT_team_goals_place", 
                                         "share_over1.5FT_team_goals_place", "share_over2.5FT_team_goals_place", 
                                         "share_over0.5HT_team_goals_place", "share_over1.5HT_team_goals_place", 
                                         "share_over2.5HT_team_goals_place", "share_over0.5secondhalf_team_goals_place",
                                         "share_over1.5secondhalf_team_goals_place", "share_over2.5secondhalf_team_goals_place", 
                                         "place")]

##  merge datasets  ##
names(data_goals_place) <- names(data_goals)
data_goals <- bind_rows(data_goals, data_goals_place)

#---------------------------------------------------------#
#                         cards                           #
#---------------------------------------------------------#

data_cards <- data_full %>% 
  group_by(Div, HomeTeam) %>% 
  summarize(no_games_played = length(btts),
            team_cards_per_game = sum(c(HY, HR)) / length(btts),
            share_over0.5_team_cards = sum(HY > 0 | HR > 0) / length(btts),
            share_over1.5_team_cards = sum(HY > 1 | HR > 1) / length(btts),
            share_over2.5_team_cards = sum(HY > 2 | HR > 2) / length(btts),
            share_over3.5_team_cards = sum(HY > 3 | HR > 3) / length(btts),
            share_team_red_card = sum(HR > 0) / length(btts),
            total_cards_per_game = sum(total_cards) / length(btts),
            share_over0.5_total_cards = sum(total_cards > 0) / length(btts),
            share_over1.5_total_cards = sum(total_cards > 1) / length(btts),
            share_over2.5_total_cards = sum(total_cards > 2) / length(btts),
            share_over3.5_total_cards = sum(total_cards > 3) / length(btts),
            share_over4.5_total_cards = sum(total_cards > 4) / length(btts),
            share_over5.5_total_cards = sum(total_cards > 5) / length(btts))

data_cards_place <- data_full %>% 
  group_by(Div, HomeTeam, place) %>% 
  summarize(no_games_played = length(btts),
            team_cards_per_game_place = sum(c(HY, HR)) / length(btts),
            share_over0.5_team_cards_place = sum(HY > 0 | HR > 0) / length(btts),
            share_over1.5_team_cards_place = sum(HY > 1 | HR > 1) / length(btts),
            share_over2.5_team_cards_place = sum(HY > 2 | HR > 2) / length(btts),
            share_over3.5_team_cards_place = sum(HY > 3 | HR > 3) / length(btts),
            share_team_red_card_place = sum(HR > 0) / length(btts),
            total_cards_per_game_place = sum(total_cards) / length(btts),
            share_over0.5_total_cards_place = sum(total_cards > 0) / length(btts),
            share_over1.5_total_cards_place = sum(total_cards > 1) / length(btts),
            share_over2.5_total_cards_place = sum(total_cards > 2) / length(btts),
            share_over3.5_total_cards_place = sum(total_cards > 3) / length(btts),
            share_over4.5_total_cards_place = sum(total_cards > 4) / length(btts),
            share_over5.5_total_cards_place = sum(total_cards > 5) / length(btts))

##  add column place to first data set  ##
data_cards$place <- paste0("all")

## change position in order to have both datasets equal
data_cards_place <- data_cards_place[, c("Div", "HomeTeam", "no_games_played", "team_cards_per_game_place", "share_over0.5_team_cards_place", 
                                         "share_over1.5_team_cards_place", "share_over2.5_team_cards_place", 
                                         "share_over2.5_team_cards_place",
                                         "share_team_red_card_place", "total_cards_per_game_place", 
                                         "share_over0.5_total_cards_place", "share_over1.5_total_cards_place",
                                         "share_over2.5_total_cards_place", "share_over3.5_total_cards_place", 
                                         "share_over4.5_total_cards_place", "share_over5.5_total_cards_place", "place")]

##  merge datasets  ##
names(data_cards_place) <- names(data_cards)
data_cards <- bind_rows(data_cards, data_cards_place)

#---------------------------------------------------------#
#                        corners                          #
#---------------------------------------------------------#

data_corners <- data_full %>% 
  group_by(Div, HomeTeam) %>% 
  summarize(no_games_played = length(btts),
            team_corners_per_game = sum(HC) / length(btts),
            share_over0.5_team_corners = sum(HC > 0) / length(btts),
            share_over1.5_team_corners = sum(HC > 1) / length(btts),
            share_over2.5_team_corners = sum(HC > 2) / length(btts),
            share_over3.5_team_corners = sum(HC > 3) / length(btts),
            share_over4.5_team_corners = sum(HC > 4) / length(btts),
            share_over5.5_team_corners = sum(HC > 5) / length(btts),
            share_over6.5_team_corners = sum(HC > 6) / length(btts),
            share_over7.5_team_corners = sum(HC > 7) / length(btts),
            share_over8.5_team_corners = sum(HC > 8) / length(btts),
            total_corners_per_game = sum(total_corners) / length(btts),
            share_over0.5_total_corners = sum(total_corners > 0) / length(btts),
            share_over1.5_total_corners = sum(total_corners > 1) / length(btts),
            share_over2.5_total_corners = sum(total_corners > 2) / length(btts),
            share_over3.5_total_corners = sum(total_corners > 3) / length(btts),
            share_over4.5_total_corners = sum(total_corners > 4) / length(btts),
            share_over5.5_total_corners = sum(total_corners > 5) / length(btts),
            share_over6.5_total_corners = sum(total_corners > 6) / length(btts),
            share_over7.5_total_corners = sum(total_corners > 7) / length(btts),
            share_over8.5_total_corners = sum(total_corners > 8) / length(btts),
            share_over9.5_total_corners = sum(total_corners > 9) / length(btts),
            share_over10.5_total_corners = sum(total_corners > 10) / length(btts),
            share_over11.5_total_corners = sum(total_corners > 11) / length(btts),
            share_over12.5_total_corners = sum(total_corners > 12) / length(btts))

data_corners_place <- data_full %>% 
  group_by(Div, HomeTeam, place) %>% 
  summarize(no_games_played = length(btts),
            team_corners_per_game_place = sum(HC) / length(btts),
            share_over0.5_team_corners_place = sum(HC > 0) / length(btts),
            share_over1.5_team_corners_place = sum(HC > 1) / length(btts),
            share_over2.5_team_corners_place = sum(HC > 2) / length(btts),
            share_over3.5_team_corners_place = sum(HC > 3) / length(btts),
            share_over4.5_team_corners_place = sum(HC > 4) / length(btts),
            share_over5.5_team_corners_place = sum(HC > 5) / length(btts),
            share_over6.5_team_corners_place = sum(HC > 6) / length(btts),
            share_over7.5_team_corners_place = sum(HC > 7) / length(btts),
            share_over8.5_team_corners_place = sum(HC > 8) / length(btts),
            total_corners_per_game_place = sum(total_corners) / length(btts),
            share_over0.5_total_corners_place = sum(total_corners > 0) / length(btts),
            share_over1.5_total_corners_place = sum(total_corners > 1) / length(btts),
            share_over2.5_total_corners_place = sum(total_corners > 2) / length(btts),
            share_over3.5_total_corners_place = sum(total_corners > 3) / length(btts),
            share_over4.5_total_corners_place = sum(total_corners > 4) / length(btts),
            share_over5.5_total_corners_place = sum(total_corners > 5) / length(btts),
            share_over6.5_total_corners_place = sum(total_corners > 6) / length(btts),
            share_over7.5_total_corners_place = sum(total_corners > 7) / length(btts),
            share_over8.5_total_corners_place = sum(total_corners > 8) / length(btts),
            share_over9.5_total_corners_place = sum(total_corners > 9) / length(btts),
            share_over10.5_total_corners_place = sum(total_corners > 10) / length(btts),
            share_over11.5_total_corners_place = sum(total_corners > 11) / length(btts),
            share_over12.5_total_corners_place = sum(total_corners > 12) / length(btts))

##  add column place to first data set  ##
data_corners$place <- paste0("all")

## change position in order to have both datasets equal
data_corners_place <- data_corners_place[, c("Div", "HomeTeam", "no_games_played", "team_corners_per_game_place", 
                                             "share_over0.5_team_corners_place", "share_over1.5_team_corners_place", 
                                             "share_over2.5_team_corners_place", "share_over3.5_team_corners_place", 
                                             "share_over4.5_team_corners_place", "share_over5.5_team_corners_place", 
                                             "share_over6.5_team_corners_place", "share_over7.5_team_corners_place", 
                                             "share_over8.5_team_corners_place", "total_corners_per_game_place", 
                                             "share_over0.5_total_corners_place", "share_over1.5_total_corners_place", 
                                             "share_over2.5_total_corners_place", "share_over3.5_total_corners_place", 
                                             "share_over4.5_total_corners_place", "share_over5.5_total_corners_place", 
                                             "share_over6.5_total_corners_place", "share_over7.5_total_corners_place", 
                                             "share_over8.5_total_corners_place", "share_over9.5_total_corners_place", 
                                             "share_over10.5_total_corners_place", "share_over11.5_total_corners_place", 
                                             "share_over12.5_total_corners_place", "place")]

##  merge datasets  ##
names(data_corners_place) <- names(data_corners)
data_corners <- bind_rows(data_corners, data_corners_place)

#---------------------------------------------------------#
#                        referees                         #
#---------------------------------------------------------#
data_ref <- data_full_ref %>% 
  group_by(Referee) %>% 
  summarize(no_games_refereed = length(Referee),
            total_cards_per_game = sum(total_cards) / length(Referee),
            yellow_cards_per_game = sum(total_yellow) / length(Referee),
            red_cards_per_game = sum(total_red) / length(Referee),
            share_over0.5_yellow_cards = sum(total_yellow > 0) / length(Referee),
            share_over1.5_yellow_cards = sum(total_yellow > 1) / length(Referee),
            share_over2.5_yellow_cards = sum(total_yellow > 2) / length(Referee),
            share_over3.5_yellow_cards = sum(total_yellow > 3) / length(Referee),
            share_over4.5_yellow_cards = sum(total_yellow > 4) / length(Referee),
            share_over5.5_yellow_cards = sum(total_yellow > 5) / length(Referee),
            share_over0.5_red_cards = sum(total_red > 0) / length(Referee))
