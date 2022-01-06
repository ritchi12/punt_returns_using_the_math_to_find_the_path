##############################
##  PACKAGES AND LIBRARIES  ##
##############################
load.libraries = c("tidyverse", "gganimate", "sp", "formattable", "magick", "kableExtra", 
                   "nflfastR", "ggpubr", "here", "magrittr", "tictoc", "ggpmisc", "plotly", "data.table")

install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {install.packages(libs, dependencies = TRUE)}
sapply(load.libraries, require, character = TRUE)



#####################
##  LOAD DATASETS  ##
#####################

## READ IN FULLDATA FOR 2020
fulldata = read_csv("Data Standardization/Data/fulldata20.csv")

## RESULTS FROM MODEL AND TWO FILES FOR SPOTLIGHT PLAYS
results = read_csv("Models/Data/New/results.csv")
ol = read_csv("Models/Data/New/ol_play.csv")
agnew = read_csv("Models/Data/New/ag_play.csv")

## NFL.COM PLAYER STATISTICS
playerStats = read_csv("Models/Data/Returner Player Stats2.csv")

##  FRECHET DISTANCES FROM OPTIMAL PATH MODEL
fre_rank = read_csv("Models/Data/New/rankings2020.csv")
colnames(fre_rank) = c("displayName", "med_path_dev", "frech_rank")



########################
##  DATA PREPARATION  ##
########################


## OBTAIN RETURN TEAM ABBREVIATIONS
returnTeam = fulldata %>%
  select(gameId, playId, possessionTeam, homeTeamAbbr, visitorTeamAbbr) %>%
  mutate(returnTeam = ifelse(homeTeamAbbr == possessionTeam, visitorTeamAbbr, homeTeamAbbr)) %>%
  select(gameId, playId, returnTeam) %>%
  unique()

## RETURNER IDS
retID = fulldata %>%
  select(gameId, playId, returnerId) %>%
  unique()

## RETURNER AND TEAMs
returnerTeams = inner_join(returnTeam, retID)


## PLAYER HEADSHOTS 
pics = fast_scraper_roster(2020) %>%
  select(full_name, pff_id, headshot_url) 


## SAVED FILE WITH RETURNER NAME, TEAM, AND ID
returnerTeams = read_csv("C:/Users/Ryker/Desktop/NFL BDB/Data/returnerTeams.csv") 
returnerTeams$returnerId = as.character(returnerTeams$returnerId)



## JOIN DATA AND CACULATE RYAE
dat = rbind(results, ol, agnew) %>%
  inner_join(returnTeam) %>%
  inner_join(retID) %>%
  mutate(RYAE = round((return_yds_remaining - cond_median),2))



##############################
##  FORMAT PLAYER RANKINGS  ##
##############################

playerData = dat %>%
  select(gameId, playId, returnTeam, returnerId, RYAE)%>%
  unique() %>%
  inner_join(returnerTeams) %>%
  arrange(displayName) %>%
  select(displayName, returnTeam, RYAE)

keys = colnames(playerData)[!grepl('RYAE',colnames(playerData))]
playerDat = as.data.table(playerData)
playerDat = playerDat[,list(RYAE2= mean(RYAE)),keys]

playerDat = playerDat %>%
  arrange(desc(RYAE2)) %>%
  inner_join(playerStats, by =c("displayName" = "Player Name")) %>%
  filter(Returns > 10)

playerDat = playerDat %>%
  mutate(RYAE3 = round(RYAE2, 2)) %>%
  mutate(Rank = c(1:nrow(playerDat))) %>%
  inner_join(fre_rank) %>%
  mutate(med_path_dev2 = round(med_path_dev, 2)) %>%
  mutate (` ` = "") %>% 
  arrange(Rank) %>%
  select(Rank, ` `,displayName, returnTeam, Average, Returns,RYAE3, med_path_dev2) 

colnames(playerDat) = c("Rank", " ", "Player Name","Team", "Average Return Yards", "Returns", "RYAE", "Average Path Deviation")




########################################
##  CREATE TABLE FOR PLAYER RANKINGS  ##
########################################

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"

playerDat$RYAE = color_tile("transparent", customGreen)(playerDat$RYAE)
playerDat$`Average Path Deviation`= color_tile(customGreen, "transparent")(playerDat$`Average Path Deviation`)

player_pics = pics %>%
  filter(full_name %in% c("Jamal Agnew", "Gunner Olszewski", "Diontae Spencer", "Deonte Harris", "Ray-Ray McCloud", "Jabrill Peppers", 
                          "Alex Erickson", "Jakeem Grant", "Mecole Hardman", "David Moore", "Brandon Powell", "K.J. Hill", "Andre Roberts", 
                          "Steven Sims", "Nyheim Hines", "James Proche", "CeeDee Lamb", "Kalif Raymond", "Nsimba Webster", "Jaydon Mickens", 
                          "Greg Ward", "Kenjon Barner", "Christian Kirk", "Marquez Callaway", "Hunter Renfrow", "Pharoh Cooper", 
                          "Donovan Peoples-Jones"))%>%
  inner_join(playerDat, by = c("full_name" = "Player Name")) %>%
  arrange(Rank) %>%
  select(full_name,headshot_url)


## TABLE 1: 1-10
playerDat[c(1:10),] %>% 
  kable(caption = "2020 Punt Return Rankings: 1-10",booktabs = T, align="c", escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, image = spec_image(player_pics$headshot_url[c(1:10)], 175,125)) %>%
  column_spec(3, width = "5cm", bold = T) %>%
  column_spec(4,bold = T, width = "3cm",border_right = "2px solid gray") %>%
  column_spec(5, width = "3cm", bold = T) %>%
  column_spec(6, width = "2.3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(7, width = "3cm", bold = T) %>%
  column_spec(8, width = "3cm", bold = T) 


## TABLE 2: 11-20
playerDat[c(11:20),] %>% 
  kable(caption = "2020 Punt Return Rankings: 11-20",booktabs = T, align="c", escape = F) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, image = spec_image(player_pics$headshot_url[c(11:20)], 175,125)) %>%
  column_spec(3, width = "5cm", bold = T) %>%
  column_spec(4,bold = T, width = "3cm",border_right = "2px solid gray") %>%
  column_spec(5, width = "3cm", bold = T) %>%
  column_spec(6, width = "2.3cm", bold = T, border_right = "2px solid gray") %>%
  column_spec(7, width = "3cm", bold = T) %>%
  column_spec(8, width = "3cm", bold = T) 




