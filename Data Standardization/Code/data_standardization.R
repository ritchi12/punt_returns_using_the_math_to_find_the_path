
##############################
##  PACKAGES AND LIBRARIES  ##
##############################

load.libraries = c("tidyverse")

install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {install.packages(libs, dependencies = TRUE)}
sapply(load.libraries, require, character = TRUE)



#####################
##  LOAD DATASETS  ##
#####################

# Must download these from the 2022 NFL Big Data Bowl Kaggle page
games = read_csv("Data Standardization/Data/games.csv")
players = read_csv("Data Standardization/Data/players.csv")
plays = read_csv("Data Standardization/Data/plays.csv")
pff = read_csv("Data Standardization/Data/PFFScoutingData.csv")
track18 = read_csv("Data Standardization/Data/tracking2018.csv")
track19 = read_csv("Data Standardization/Data/tracking2019.csv")
track20 = read_csv("Data Standardization/Data/tracking2020.csv")




######################################################
##  STADARDIZE DATA AND JOIN ALL DATASETS INTO ONE  ##
######################################################

# Standardize years separately to avoid overcomputation

# 2018
fulldata18 = games %>%
  inner_join(plays) %>%
  inner_join(pff) %>%
  inner_join(track18) %>% 
  mutate(home_possesion = ifelse(homeTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
  mutate(away_possesion = ifelse(visitorTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
  mutate(x_std = ifelse(playDirection == "left", 120-x, x), y_std = ifelse(playDirection == "left", 160/3 - y, y)) %>%
  mutate(dir_std = ifelse(playDirection == "left" & dir < 180, dir+180, ifelse(playDirection == "left" & dir > 180,dir-180, dir))) %>%
  filter(specialTeamsPlayType == "Punt") %>% 
  filter(specialTeamsResult=="Return")

write.csv("Data Standardization/Data/fulldata18.csv")

# 2019
fulldata19 = games %>%
  inner_join(plays) %>%
  inner_join(pff) %>%
  inner_join(track19) %>% 
  mutate(home_possesion = ifelse(homeTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
  mutate(away_possesion = ifelse(visitorTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
  mutate(x_std = ifelse(playDirection == "left", 120-x, x), y_std = ifelse(playDirection == "left", 160/3 - y, y)) %>%
  mutate(dir_std = ifelse(playDirection == "left" & dir < 180, dir+180, ifelse(playDirection == "left" & dir > 180,dir-180, dir))) %>%
  filter(specialTeamsPlayType == "Punt") %>% 
  filter(specialTeamsResult=="Return")

write.csv("Data Standardization/Data/fulldata19.csv")

# 2020
fulldata20 = games %>%
  inner_join(plays) %>%
  inner_join(pff) %>%
  inner_join(track20) %>% 
  mutate(home_possesion = ifelse(homeTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
  mutate(away_possesion = ifelse(visitorTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
  mutate(x_std = ifelse(playDirection == "left", 120-x, x), y_std = ifelse(playDirection == "left", 160/3 - y, y)) %>%
  mutate(dir_std = ifelse(playDirection == "left" & dir < 180, dir+180, ifelse(playDirection == "left" & dir > 180,dir-180, dir))) %>%
  filter(specialTeamsPlayType == "Punt") %>% 
  filter(specialTeamsResult=="Return")

write.csv("Data Standardization/Data/fulldata20.csv")


