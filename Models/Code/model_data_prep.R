
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

## FULL TRACKING DATA SETS ##
fulldata18 = read_csv("Data Standardization/Data/fulldata18.csv")
fulldata19 = read_csv("Data Standardization/Data/fulldata19.csv")
fulldata20 = read_csv("Data Standardization/Data/fulldata20.csv")


## DATA FROM OTHER ANALYSIS ##
frechet = read_csv("Optimal Path/Data/New/metrics.csv")
hulls = read_csv("Optimal Path/Data/New/hull_area.csv") 


for (year in c(2018, 2019, 2020)) {
  
  if (year == 2018) {
    fulldata = fulldata18
  } else if (year == 2019) {
    fulldata = fulldata19
  } else {
    fulldata = fulldata20
  }
  
  ######################################################
  ##  STADARDIZE DATA AND JOIN ALL DATASETS INTO ONE  ##
  ######################################################
  
  fulldata = games %>%
    inner_join(plays) %>%
    inner_join(pff) %>%
    
    ##computer only handled one year at a time, switch this in for track19 and track20 to get fulldata 
    inner_join(track18)%>% 
    
    mutate(home_possesion = ifelse(homeTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
    mutate(away_possesion = ifelse(visitorTeamAbbr == possessionTeam, TRUE, FALSE)) %>%
    mutate(x_std = ifelse(playDirection == "left", 120-x, x), y_std = ifelse(playDirection == "left", 160/3 - y, y)) %>%
    mutate(dir_std = ifelse(playDirection == "left" & dir < 180, dir+180, ifelse(playDirection == "left" & dir > 180,dir-180, dir))) %>%
    filter(specialTeamsPlayType == "Punt") %>% 
    filter(specialTeamsResult=="Return")
  
  
  ##################################
  ##  FILTER FOR ONLY PUNT PLAYS  ##
  ##################################
  
  punt_sample =  fulldata %>%
    filter(specialTeamsPlayType == "Punt") %>% 
    filter(specialTeamsResult=="Return") %>% 
    #filter(is.na(penaltyYards) == TRUE) %>%
    group_by(gameId, playId, frameId, event) %>%
    nest()
  
  
  ##########################################################################################
  
  
  ###########################################
  ## STEP 1: DATA PREPARATION AND CLEANING ##
  ###########################################
  
  punt_data = punt_sample %>%
    group_by(gameId, playId) %>% 
    nest() %>%
    mutate(data = map(data, ~.x %>%
                        mutate(punt_received = ifelse(event == "punt_received", 1, 0)) %>%
                        mutate(ball_received = cumsum(punt_received)) %>%
                        select(-punt_received))) %>%
    unnest() %>% ungroup() %>%
    mutate(data = map2(data, ball_received, ~.x %>% 
                         # Add in ball_received column
                         mutate(ball_received = .y) %>%
                         # Identify returner, returning team and kicking team for each frame
                         mutate(ball_x = .$x_std[.$displayName == "football"],
                                ball_y = .$y_std[.$displayName == "football"],
                                dist_to_ball = sqrt((x_std - ball_x)^2 + (y_std - ball_y)^2)) %>%
                         mutate(player_type = case_when(
                           displayName == "football" ~ "Football",
                           dist_to_ball == min(.$dist_to_ball[.$displayName != "football"]) & ball_received == 1 ~ "Returner",
                           x_std == max(.$x_std[.$displayName != "football"]) & ball_received == 0 ~ "Returner",
                           (team == "home" & home_possesion == FALSE) | (team == "away" & away_possesion == FALSE) ~ "Returning Team",
                           TRUE ~ "Kicking Team"
                         )) %>%
                         # Further cleaning and prepping
                         mutate(x_coor = x_std, y_coor = y_std) %>%
                         select(nflId, player_type, jerseyNumber, displayName, x_coor, y_coor) %>%
                         arrange(player_type) %>%
                         mutate(type_code = case_when(
                           player_type == "Football" ~ "F",
                           player_type == "Returner" ~ "PR",
                           player_type == "Returning Team" ~ "RT",
                           player_type == "Kicking Team" ~ "KT"
                         )) %>%
                         mutate(jerseyNumber = ifelse(is.na(jerseyNumber), "", jerseyNumber)) %>%
                         mutate(player_code = paste(type_code, jerseyNumber, sep = ""))))
  
  
  
  ####################################################################
  ## STEP 2: EUCLIDEAN DISTANCES OF KICKING TEAM TO CLOSEST BLOCKER ##
  ####################################################################
  
  punt_data2 = punt_data %>%
    filter(!gameId %in% c(2020092004)) ## issue with this game
  
  punt_df = punt_data2 %>%
    # Build new nested df with just kicking team info (for Delaunay triangulation)
    mutate(kick_team_data = map(data, ~.x %>% filter(player_type == "Kicking Team"))) %>%
    # Create Euclidean distance matrix for all players
    mutate(euclidean_full = map(data, ~dist(.x %>% select(x_coor, y_coor)) %>%
                                  as.matrix() %>% data.frame() %>%
                                  set_colnames(.x$player_code) %>%
                                  mutate(player_code = .x$player_code))) %>%
    # Get the closest blocker for each member of the kicking team
    mutate(closest_blocker = map(euclidean_full, ~.x %>% 
                                   filter(grepl("KT", player_code)) %>% 
                                   select_at(vars(starts_with(c("PR", "RT", "player")))) %>%
                                   pivot_longer(cols = starts_with(c("PR", "RT")), names_to = "blocker", values_to = "dist") %>%
                                   group_by(player_code) %>% 
                                   slice(which.min(dist)) )) %>%
    # Add closest blocker to neighbors data frame
    mutate(kick_team_data = map2(kick_team_data, closest_blocker, ~.x %>% 
                                   left_join(.y, by = c("player_code")) %>%
                                   rename(blocker_dist = dist)
    )) %>%
    # Add blocker locations
    mutate(kick_team_data = map2(kick_team_data, data, ~.x %>%
                                   left_join(.y %>% select(blocker = player_code, blocker_x = x_coor, blocker_y = y_coor), by = c("blocker")) )) %>%
    
    select(gameId, playId, frameId, event, kick_team_data) %>%
    unnest(kick_team_data) %>% ungroup() %>%
    mutate(blocker_x = as.numeric(blocker_x), blocker_y = as.numeric(blocker_y)) %>%
    mutate(blocker_dist = sqrt((x_coor - blocker_x)^2 + (y_coor - blocker_y)^2)) %>%
    mutate(blocked = ifelse(blocker_dist < 2, 1, 0)) %>%
    arrange(playId, frameId, jerseyNumber)
  
  
  
  #############################################################
  ## STEP 3: EUCLIDEAN DISTANCES OF KICKING TEAM TO RETURNER ##
  #############################################################
  
  ## RETURNER
  returner_data = fulldata %>%
    filter(specialTeamsPlayType == "Punt") %>%
    filter(is.na(returnerId) == FALSE) %>%
    filter(str_count(returnerId) < 7) %>%
    filter(team != "football") %>%
    filter(returnerId == nflId) %>%
    mutate(x_returner = x_std) %>%
    mutate(y_returner = y_std) %>%
    mutate(speed_ret = s) %>%
    mutate(dir_ret = dir_std) %>%
    select(gameId, playId, frameId, event, x_returner, y_returner, speed_ret, dir_ret) %>%
    arrange(playId, frameId) 
  
  
  ## ADD DISTNACES TO MAIN DATA FRAME ##
  punt_df = punt_df %>%
    inner_join(returner_data)
  
  punt_df = punt_df %>%
    mutate(returner_dist = sqrt((x_coor - x_returner)^2 + (y_coor - y_returner)^2)) %>%
    mutate(def_behind = ifelse((x_coor - x_returner) > 0, 1, 0))
  
  
  
  ##############################
  ## STEP 4: BLOCKER LEVERAGE ##
  ##############################
  
  ## FIND EXPECTED POSITION OF RETURNER AFTER 0.5s
  punt_df = punt_df %>%
    mutate(angle = ifelse(dir_ret < 90, 90-dir_ret, ifelse(dir_ret > 90 & dir_ret < 180, dir_ret-90, ifelse(dir_ret > 180 & dir_ret < 270, 270-dir_ret, dir_ret-270)))) %>%
    mutate(x_change = ifelse(dir_ret < 180, sin((angle*pi)/180)*(speed_ret/2), -sin((angle*pi)/180)*(speed_ret/2))) %>%
    mutate(y_change = ifelse(dir_ret > 90 & dir_ret < 270, -cos((angle*pi)/180)*(speed_ret/2), cos((angle*pi)/180)*(speed_ret/2))) %>%
    mutate(x_ret_exp = x_returner+x_change) %>%
    mutate(y_ret_exp = y_returner+y_change)
  
  
  pRB = sqrt((punt_df$x_ret_exp - punt_df$blocker_x)^2 + (punt_df$y_ret_exp - punt_df$blocker_y)^2)
  pRD = sqrt((punt_df$x_ret_exp - punt_df$x_coor)^2 + (punt_df$y_ret_exp - punt_df$y_coor)^2)
  pBD = sqrt((punt_df$x_coor - punt_df$blocker_x)^2 + (punt_df$y_coor - punt_df$blocker_y)^2)
  
  block_leverage = acos((pRB^2 + pRD^2 - pBD^2)/(2*pRB*pRD))
  punt_df$block_leverage_deg = block_leverage*180/pi
  
  
  
  ###############################
  ## STEP 5: DEFENDER LEVERAGE ##
  ###############################
  
  DRp = sqrt((punt_df$x_ret_exp - punt_df$x_coor)^2 + (punt_df$y_ret_exp - punt_df$y_coor)^2)
  DR = sqrt((punt_df$x_returner - punt_df$x_coor)^2 + (punt_df$y_returner - punt_df$y_coor)^2)
  DB = sqrt((punt_df$x_coor - punt_df$blocker_x)^2 + (punt_df$y_coor - punt_df$blocker_y)^2)
  RB = sqrt((punt_df$x_returner - punt_df$blocker_x)^2 + (punt_df$y_returner - punt_df$blocker_y)^2)
  RR = sqrt((punt_df$x_returner - punt_df$x_ret_exp)^2 + (punt_df$y_returner - punt_df$y_ret_exp)^2)
  
  theta1 = acos((DB^2 + DR^2 - RB^2)/(2*DB*DR))
  theta2 = acos((DR^2 + DRp^2 - RR^2)/(2*DR*DRp))
  
  delta = theta2-theta1
  punt_df$defender_leverage_deg = delta*180/pi
  
  
  
  ########################################
  ## STEP 6: SET UP DATAFRAME FOR MODEL ##
  ########################################
  
  ## COLUMN NAMES
  names = c("gameId", "playId", "frameId", "event", "x_returner", "y_returner","speed_ret", "dir_ret" ,"ret_dist1","ret_dist2","ret_dist3","ret_dist4","ret_dist5","ret_dist6","ret_dist7","ret_dist8","ret_dist9","ret_dist10","ret_dist11", "blocked1","blocked2","blocked3","blocked4","blocked5","blocked6","blocked7","blocked8","blocked9","blocked10","blocked11","block_dist1", "block_dist2", "block_dist3", "block_dist4", "block_dist5", "block_dist6", "block_dist7", "block_dist8", "block_dist9", "block_dist10", "block_dist11", "blocker_leverage1", "blocker_leverage2","blocker_leverage3","blocker_leverage4","blocker_leverage5","blocker_leverage6","blocker_leverage7","blocker_leverage8","blocker_leverage9","blocker_leverage10","blocker_leverage11","defender_leverage1", "defender_leverage2", "defender_leverage3","defender_leverage4","defender_leverage5","defender_leverage6","defender_leverage7","defender_leverage8","defender_leverage9","defender_leverage10","defender_leverage11","def_behind1","def_behind2","def_behind3","def_behind4","def_behind5","def_behind6","def_behind7","def_behind8","def_behind9","def_behind10","def_behind11")
  
  
  punt_df2 = punt_df %>%
    select(gameId, playId, frameId, event, x_returner, y_returner, speed_ret, dir_ret, returner_dist, blocked, blocker_dist, block_leverage_deg, defender_leverage_deg, def_behind) %>%
    arrange(gameId, playId,frameId, returner_dist) %>%
    group_by(gameId, playId,frameId) %>%
    nest() %>%
    mutate(data = map(data, ~.x %>% mutate(order = 1:nrow(.)))) %>%
    unnest() %>%
    ungroup()
  
  
  # FOR 2020 ONLY: ISSUE WITH A PLAY
  #punt_df2 = punt_df2 %>% filter(!playId %in% 2142)
  
  
  ## PIVOT TABLE TO OBTAIN 1 ROW PER FRAME
  df = data.frame()
  g = unique(punt_df2 %>% select(gameId, playId))
  for(i in 1:nrow(g)){
    data = punt_df2 %>%
      filter(gameId == g$gameId[i]) %>%
      filter(playId == g$playId[i]) %>%
      pivot_wider(names_from = order, values_from = c(returner_dist,blocked, blocker_dist, block_leverage_deg, defender_leverage_deg, def_behind))
    
    tackle = ifelse(nrow(data %>% filter(event == 'tackle')) > 0, which(data$event == "tackle"), ifelse(nrow(data %>% filter(event == 'out_of_bounds')) > 0, which(data$event == "out_of_bounds"),ifelse(nrow(data %>% filter(event == 'touchdown')) > 0, which(data$event == "touchdown"),ifelse(nrow(data %>% filter(event == 'fumble')) > 0, which(data$event == "fumble"), nrow(data)))))
    if(length(which(data$event == "punt_received") > 0)) {                                                 
      data = data[which(data$event == "punt_received"):tackle,]
      colnames(data) = names
      df = rbind(df, data) 
    }
  }
  
  ## ADD OTHER PFF DATA AND ADD RESPONSE VARIABLE (RETURN YARDS REMAINING)
  play_info = fulldata %>%
    select(gameId, playId, frameId,returnerId, possessionTeam, yardlineNumber, penaltyYards, kickLength, kickReturnYardage, playResult, absoluteYardlineNumber, snapTime, operationTime, hangTime, missedTackler, tackler, gunners, puntRushers, specialTeamsSafeties, vises) %>% unique()
  
  model_data = df %>%
    inner_join(play_info) %>%
    mutate(numMissedTackler = ifelse(is.na(missedTackler) == TRUE,0,str_count(missedTackler, ';')+1)) %>%
    mutate(numVises = ifelse(is.na(vises) == TRUE,0,str_count(vises, ';')+1)) %>%
    mutate(numGunners = ifelse(is.na(gunners) == TRUE,0,str_count(gunners, ';')+1)) %>%
    mutate(numPuntRushers = ifelse(is.na(puntRushers) == TRUE,0,str_count(puntRushers, ';')+1)) %>%
    mutate(num_Safeties=ifelse(is.na(specialTeamsSafeties) == TRUE,0,str_count(specialTeamsSafeties, ';')+1)) %>% 
    mutate(numPlayersBlocked = blocked1+ blocked2+ blocked3+ blocked4+ blocked5+ blocked6+ blocked7+ blocked8+ ifelse(is.na(blocked9) == TRUE, 0, blocked9)+ ifelse(is.na(blocked10) == TRUE, 0, blocked10)+ ifelse(is.na(blocked11) == TRUE, 0, blocked11)) %>%
    mutate(blocker_lev_wt1 = blocked1*blocker_leverage1) %>%
    mutate(blocker_lev_wt2 = blocked2*blocker_leverage2) %>%
    mutate(blocker_lev_wt3 = blocked3*blocker_leverage3) %>%
    mutate(blocker_lev_wt4 = blocked4*blocker_leverage4) %>%
    mutate(blocker_lev_wt5 = blocked5*blocker_leverage5) %>%
    mutate(blocker_lev_wt6 = blocked6*blocker_leverage6) %>%
    mutate(blocker_lev_wt7 = blocked7*blocker_leverage7) %>%
    mutate(blocker_lev_wt8 = blocked8*blocker_leverage8) %>%
    mutate(blocker_lev_wt9 = blocked9*blocker_leverage9) %>%
    mutate(blocker_lev_wt10 = blocked10*blocker_leverage10) %>%
    mutate(blocker_lev_wt11 = blocked11*blocker_leverage11) %>%
    mutate(defender_lev_wt1 = blocked1*defender_leverage1) %>%
    mutate(defender_lev_wt2 = blocked2*defender_leverage2) %>%
    mutate(defender_lev_wt3 = blocked3*defender_leverage3) %>%
    mutate(defender_lev_wt4 = blocked4*defender_leverage4) %>%
    mutate(defender_lev_wt5 = blocked5*defender_leverage5) %>%
    mutate(defender_lev_wt6 = blocked6*defender_leverage6) %>%
    mutate(defender_lev_wt7 = blocked7*defender_leverage7) %>%
    mutate(defender_lev_wt8 = blocked8*defender_leverage8) %>%
    mutate(defender_lev_wt9 = blocked9*defender_leverage9) %>%
    mutate(defender_lev_wt10 = blocked10*defender_leverage10) %>%
    mutate(defender_lev_wt11 = blocked11*defender_leverage11) %>%
    group_by(gameId, playId) %>%
    nest() %>%
    mutate(data = map(data, ~.x %>%
                        mutate(diff = lag(x_returner) - x_returner) %>%
                        mutate(diff = ifelse(is.na(diff), 0, diff)) %>%
                        mutate(return_yardage = cumsum(diff)))) %>%
    unnest() %>% ungroup() %>%
    mutate(return_yds_remaining = kickReturnYardage - return_yardage )
  
  
  
  ###########################################
  ## STEP 7: JOIN IN FRECHET AND HULL DATA ##
  ###########################################
  
  model_data2 = model_data %>%
    inner_join(frechet) %>%
    inner_join(hulls)
  
  
  
  ###########################
  ##  STEP 8:  SAVE FILES  ## 
  ###########################
  
  
  fwrite(model_data2, paste("Models/Data/New/modeldata", year, ".csv", sep = ""))
  
  
  
}

