

##########################################
#### STEP 0.1: LOAD DATA AND PACKAGES ####
##########################################

# Load (and install if necessary) packages
load.libraries = c("tripack", "tidyverse", "gganimate", "sp", "StereoMorph", "janitor", "splancs", 
                   "reshape2", "sf", "kmlShape", "furrr", "tictoc", "ggpmisc", "plotly")
install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {install.packages(libs, dependencies = TRUE)}
sapply(load.libraries, require, character = TRUE)

# Load punt returns from the tracking data
punt_returns = read_csv("Data Standardization/Data/fulldata20.csv") %>% # Created in Data Standardization folder
  bind_rows(read_csv("Data Standardization/Data/fulldata19.csv")) %>% # Created in Data Standardization folder
  bind_rows(read_csv("Data Standardization/Data/fulldata18.csv")) %>% # Created in Data Standardization folder
  filter(specialTeamsPlayType == "Punt") %>% 
  filter(specialTeamsResult == "Return") %>% 
  mutate(available_dist = kickLength/(120-yardlineNumber)) %>%
  mutate(kteamname = possessionTeam) %>%
  mutate(rteamname = ifelse(homeTeamAbbr == possessionTeam, visitorTeamAbbr, homeTeamAbbr)) %>%
  group_by(season, gameId, playId, frameId, event, kteamname, rteamname) %>%
  nest() %>%
  ungroup()

# Load colours
colours = read_csv("Optimal Path/Data/nfl_cols.csv", col_names = FALSE)

# Source in functions required to run code
source("Code/pipeline_functions.R")

# Set number of cores to use (max of available cores - 4 and 2)
plan(multisession, workers = max(availableCores() - 4, 2))



####################################
#### STEP 0.2: DATA PREPARATION ####
####################################

tic()
# Filter for frames from punt receival to end of play
punt_df_v0 = punt_returns %>%
  # Filter for all frames during active punt return (between punt receival and end of play)
  filter((gameId == 2020112906 & playId == 2142) | (gameId == 2020092007 & playId == 1589) ) %>%
  group_by(season, gameId, playId) %>%
  mutate(punt_received_on_frame = ifelse(event == "punt_received", 1, 0)) %>%
  mutate(punt_received = cumsum(punt_received_on_frame)) %>%
  mutate(tackle_on_frame = ifelse(lag(event) == "tackle", 1, 0)) %>%
  mutate(tackle_on_frame = ifelse(is.na(tackle_on_frame), 0, tackle_on_frame)) %>%
  mutate(tackle = cumsum(tackle_on_frame)) %>%
  mutate(out_of_bounds_on_frame = ifelse(lag(event) == "out_of_bounds", 1, 0)) %>%
  mutate(out_of_bounds_on_frame = ifelse(is.na(out_of_bounds_on_frame), 0, out_of_bounds_on_frame)) %>%
  mutate(out_of_bounds = cumsum(out_of_bounds_on_frame)) %>%
  mutate(touchdown_on_frame = ifelse(lag(event) == "touchdown", 1, 0)) %>%
  mutate(touchdown_on_frame = ifelse(is.na(touchdown_on_frame), 0, touchdown_on_frame)) %>%
  mutate(touchdown = cumsum(touchdown_on_frame)) %>%
  ungroup() %>%
  filter(punt_received == 1 & out_of_bounds == 0 & tackle == 0 & touchdown == 0) %>%
  select(-punt_received_on_frame, -touchdown_on_frame, -tackle_on_frame, -out_of_bounds_on_frame,
         -punt_received, -touchdown, -out_of_bounds, -tackle, -touchdown) %>%
  # Clean punt data and add player types
  mutate(data = future_map(data, clean_punt_data)) %>% rename(player_locations = data) %>%
  # Create nested data frames for the carrier, blockers and defenders
  mutate(kicking_team = future_map(player_locations, ~.x %>% filter(player_type == "Kicking Team"))) %>%
  mutate(returning_team = future_map(player_locations, ~.x %>% filter(player_type == "Returning Team"))) %>%
  mutate(carrier = future_map(player_locations, ~.x %>% filter(player_type == "Returner"))) 
toc()



#################################################
#### STEP 1: CREATE WINDOWS AND ACTION ZONES ####
#################################################

tic()
punt_df_v1 = punt_df_v0 %>%
  mutate(car_len = map(carrier, ~nrow(.x))) %>% unnest(car_len) %>% filter(car_len > 0) %>% select(-car_len) %>%
  # Get all defenders above the carrier on the field
  mutate(kicking_team_above = future_map2(kicking_team, carrier, ~.x %>% filter(y_coor < .y$y_coor))) %>%
  # Get number of defenders above carrier
  mutate(defenders_above_count = future_map(kicking_team_above, ~nrow(.x))) %>% unnest(defenders_above_count) %>%
  # Create windows between defenders
  mutate(windows = future_map2(kicking_team_above, carrier, create_windows)) %>%
  # Remove sidelines
  mutate(windows = future_map(windows, ~.x %>% filter(!grepl("Sideline", window_type)))) %>%
  # Get actionable windows
  mutate(action_zone = future_map(windows, ~.x %>% filter(is_actionable == 1)))
toc()



#########################################################################
### STEP 2: FIND MINIMUM ARRIVAL TIMES OF DEFENDERS ALONG EACH WINDOW ###
#########################################################################

tic()
punt_df_v2 = punt_df_v1 %>%
  # Get point of maximum arrival time (and arrival times for up to 25 points) along each window
  mutate(windows = future_map2(windows, player_locations, possibly(find_arrival_time, NULL))) %>%
  # Additional calculate arrival time to the carrier (for model use later)
  mutate(arrival_to_carrier = map(player_locations, get_arrival_to_carrier))
toc()



###########################################################
#### STEP 3: FIND OPTIMAL PATH VIA A* SEARCH ALGORITHM ####
###########################################################

tic()
punt_df_v3 = punt_df_v2 %>%
  # Output optimal path from the A* algorithm
  mutate(optimal_path = future_map(windows, possibly(find_optimal_path, NULL))) %>%
  # Fill in optimal path with straight arrow ahead if there are no defenders above the carrier
  mutate(optimal_path = ifelse(defenders_above_count == 0, 
                               future_map(carrier, ~data.frame(x1 = .x$x_coor, y1 = .x$y_coor, x2 = .x$x_coor, y2 = .x$y_coor - 5)), 
                               optimal_path))
toc()



###########################################################################
#### STEP 4: MEASURE THE DEVIATION OF THE ACTUAL PATH AND OPTIMAL PATH ####
###########################################################################

# Get full carrier paths for each play
all_carrier_paths = punt_df_v3 %>%
  select(gameId, playId, frameId, carrier) %>%
  unnest(carrier) %>% ungroup() %>%
  group_by(gameId, playId) %>%
  nest() %>% rename(carrier_path = data)


tic()
punt_df_v4 = punt_df_v3 %>%
  # Join on carrier path data
  left_join(all_carrier_paths, by = c("gameId", "playId")) %>%
  # Smooth optimal path via Frechet distance
  group_by(season, gameId, playId) %>%
  mutate(required_smoothing = future_pmap(list(optimal_path, lead(optimal_path), lag(optimal_path)), possibly(smooth_path, NULL))) %>%
  unnest(required_smoothing) %>%
  mutate(optimal_path = ifelse(required_smoothing == 1, lag(optimal_path), optimal_path)) %>%
  ungroup() %>%
  # Determine the deviation between the actual path and the optimal path over the next 5 seconds
  mutate(path_frechet = future_pmap(list(carrier_path, optimal_path, frameId), possibly(invisible(find_path_frechet), NA_real_)))
toc()



######################################
### STEP 5: ANALYZE PATH DEVIATION ###
######################################

# Get path deviation for each play
path_deviation_by_play = punt_df_v4 %>%
  # Unpack Frechet distance at each frame (eliminates 0 and NA)
  unnest(path_frechet) %>%
  filter(path_frechet != 0) %>%
  # Unpack carrier name, ID and approximate yards on the play
  mutate(carrier_name = map(carrier, ~.x$displayName[1])) %>%
  mutate(carrier_id = map(carrier, ~.x$nflId[1])) %>%
  mutate(approx_yards = map(carrier_path, ~.x$y_coor[1] - min(.x$y_coor))) %>%
  # Select relevant columns
  select(season, gameId, playId, frameId, return_team = rteamname, kick_team = kteamname, carrier_id, carrier_name, approx_yards, path_frechet) %>%
  unnest(c(carrier_id, carrier_name, approx_yards)) %>% ungroup() %>%
  # Summarize over play to get
  group_by(season, gameId, playId, return_team, kick_team, carrier_id, carrier_name) %>%
  summarize(frame_count = max(frameId) - min(frameId), approx_yards = mean(approx_yards), 
            frechet_median = median(path_frechet), frechet_mean = mean(path_frechet)) %>%
  # Take log10 of yards
  mutate(log_yards = log(approx_yards, 10))


# Get correlation between 
corr_df = path_deviation_by_play %>% filter(approx_yards > 0)
deviation_corr = cor(x = corr_df$log_yards, y = corr_df$frechet_mean)


# Plot the approximate yards of each play vs the median Frechet distance value
p1 = path_deviation_by_play %>%
  mutate(year = str_sub(gameId, start = 1, end = 4)) %>%
  mutate(month = str_sub(gameId, start = 5, end = 6)) %>%
  mutate(day = str_sub(gameId, start = 7, end = 8)) %>%
  mutate(game_date = paste(year, month, day, sep = "-")) %>%
  mutate(`LOESS Curve` = "") %>%
  rename(`Fréchet Path Deviation` = frechet_mean, `Yards on Play` = approx_yards,
         `Punt Returner` = carrier_name, `Return Team` = return_team, `Kick Team` = kick_team,
         `Season` = season, `Date` = game_date) %>%
  mutate(`Fréchet Path Deviation` = ifelse(`Fréchet Path Deviation` >= 53.33, 53.33, `Fréchet Path Deviation`)) %>%
  ggplot() +
    geom_point(aes(x = `Yards on Play`, y = `Fréchet Path Deviation`, label = `Punt Returner`, label2 = `Return Team`, label3 = `Kick Team`, label4 = `Season`, label5 = `Date`), alpha = 0.5) +
    geom_smooth(aes(x = `Yards on Play`, y = `Fréchet Path Deviation`, label = `LOESS Curve`), se = F, size = 2, colour = "goldenrod") +
    theme_bw() +
    labs(title = "Path Deviation by Punt Return")

ggplotly(p1, tooltip = c("label", "label2", "label3", "label4", "label5"))



# Get the average path deviation and average yards for all players with >=5 attempts
path_deviation_by_carrier = path_deviation_by_play %>%
  group_by(season, carrier_name, return_team) %>%
  summarize(path_dev_med = mean(frechet_median), path_dev_mean = mean(frechet_mean), attempts = n(), yards = mean(approx_yards)) %>%
  rename(team = return_team) %>%
  ungroup() %>%
  filter(attempts >= 8) %>%
  group_by(season) %>%
  mutate(rank = rank(path_dev_med)) %>%
  ungroup() %>%
  arrange(season, path_dev_med)



# Plot the average path deviation by the average yards on the play
p2 = path_deviation_by_carrier %>%
  left_join(images, by = "team") %>%
  mutate(season = factor(season)) %>%
  rename(`Punt Returner` = carrier_name,
         `Season` = season,
         `Average Path Deviation` = path_dev_mean,
         `Average Yards` = yards,
         `Number of Attempts` = attempts,
         `Team` = team
         ) %>%
  mutate(`Line of Best Fit` = "") %>%
  ggplot() +
    geom_point(aes(label2 = `Punt Returner`, label3 = `Team`, label4 = `Number of Attempts`, colour = `Season`, x = `Average Path Deviation`, y = `Average Yards`), size = 3) +
    geom_smooth(aes(x = `Average Path Deviation`, y = `Average Yards`, label2 = `Line of Best Fit`), method = "lm", se = F, colour = "black") +
    labs(x = "Average Path Deviation", y = "Average Yards", title = "Path Deviation by Punt Returner") +
    geom_text(aes(x = 5, y = 21, label = "Good Vision,\nHigh Skill"), size = 3) +
    geom_text(aes(x = 16, y = 21, label = "Poor Vision,\nHigh Skill"), size = 3) +
    geom_text(aes(x = 5, y = 7, label = "Good Vision,\nLow Skill"), size = 3) +
    geom_text(aes(x = 16, y = 7, label = "Poor Vision,\nLow Skill"), size = 3) +
    theme_bw()

ggplotly(p2, tooltip = c("label2", "label3", "label4", "colour"))



# Save the path deviation data frames as csv's
#write.csv(path_deviation_by_play, "Optimal Path/Data/path_deviation_by_play.csv", row.names = FALSE)
#write.csv(path_deviation_by_carrier, "Optimal Path/Data/path_deviation_by_carrier.csv", row.names = FALSE)



#######################################################
#### STEP 6: FIND ADAPTIVE STOCHASTIC CONVEX HULLS ####
#######################################################

punt_df_v6 = punt_df_v4 %>%
  # Find all players in hull
  mutate(hull_players = future_map(player_locations, get_hull_players)) %>%
  # Find hull areas and store in table
  mutate(hull_table = future_map2(frameId, hull_players, get_hull_table))



###########################################
#### STEP 7: CREATE ANIMATIONS OF PLAY ####
###########################################

# Initialize plot settings
xmin = 0; xmax = 53.33
ymin = 0; ymax = 120
hash.right = 38.35
hash.left = 12
hash.width = 3.3
field.alpha = 0.7 # Alpha level of the field
options(gganimate.dev_args = list(width = 8, height = 6, units = 'in', res=320))


# Create animation within data frame
tic()
punt_df_v7 = punt_df_v6 %>%
  # Filter for touchdown plays in 2020
  filter(season == 2020) %>%
  group_by(gameId, playId) %>% filter("touchdown" %in% event) %>% ungroup() %>%
  # Create table to be displayed in plot
  mutate(path_table = future_pmap(list(frameId, path_frechet, player_locations), create_path_table)) %>%
  mutate(player_locations = future_map2(player_locations, hull_players, ~.x %>% mutate(in_hull = ifelse(player_code %in% .y$player_code, "Yes", "No")))) %>%
  # Renest by play
  group_by(gameId, playId, kteamname, rteamname) %>%
  nest() %>% rename(full_play = data) %>%
  ungroup() %>%
  # Unpack data from the play
  mutate(carrier_y_df = map(full_play, unpack_carrier)) %>%
  mutate(player_locations = pmap(list(full_play, carrier_y_df, kteamname, rteamname), unpack_player_locations)) %>%
  mutate(kicking_team_above = map2(full_play, carrier_y_df, unpack_kick_team_above)) %>%
  mutate(field_pressure = map(full_play, unpack_field_pressure)) %>%
  mutate(windows = map(full_play, unpack_windows)) %>%
  mutate(optimal_path = map2(full_play, carrier_y_df, unpack_optimal_path)) %>%
  mutate(path_table = map(full_play, unpack_path_table)) %>%
  mutate(hull_table = map(full_play, unpack_hull_table)) %>%
  mutate(kick_hull = map2(full_play, carrier_y_df, unpack_kick_hull)) %>%
  mutate(return_hull = map2(full_play, carrier_y_df, unpack_return_hull)) %>%
  select(-full_play) %>%
  # Add in extra features needed in plot
  mutate(carrier_path = map2(carrier_y_df, player_locations, create_carrier_path)) %>%
  mutate(hash_marks = map(carrier_y_df, create_hash_marks)) %>%
  mutate(yard_lines = map(carrier_y_df, create_yard_lines)) %>%
  mutate(yard_line_text = map(carrier_y_df, create_yard_line_text)) %>%
  mutate(field_outline = map(carrier_y_df, create_outline)) %>%
  mutate(team_colours = map2(kteamname, rteamname, choose_colours)) %>% # Must load in colours csv as `colours` first
  # Create plot
  mutate(path_animation = pmap(list(player_locations, windows, field_pressure, optimal_path, path_table,
                                    carrier_path, hash_marks, yard_lines, yard_line_text, field_outline, team_colours), create_path_animation)) %>%
  mutate(hull_animation = pmap(list(player_locations, kick_hull, return_hull, hull_table, 
                                    carrier_path, hash_marks, yard_lines, yard_line_text, field_outline, team_colours), create_hull_animation))
toc()


# Save animations for each play
for (i in 1:nrow(punt_df_v7)) {
  
  path_animation = punt_df_v7$path_animation[[i]]
  game = punt_df_v7$gameId[[i]]
  play = punt_df_v7$playId[[i]]
  rteam = punt_df_v7$rteamname[[i]]
  kteam = punt_df_v7$kteamname[[i]]
  max_frame = max(punt_df_v7$player_locations[[i]]$frameId)
  min_frame = min(punt_df_v7$player_locations[[i]]$frameId)
  
  print(paste("Saving ", game, "_", play, " ", rteam, " vs ", kteam, " (", i, " of ", nrow(punt_df_v7), ")", sep = ""))
  
  # # Save gif
  # tic()
  # anim_save(paste("Optimal Path/Sample Plays/", game, "_", play, "_", rteam, "vs", kteam, "_path.gif", sep = ""), path_animation,
  #           fps = 10, duration = (max_frame - min_frame)/10 + 1, end_pause = 10)
  # toc()
  
  # Save mp4
  tic()
  animation2 = animate(path_animation, renderer = ffmpeg_renderer(), fps = 10, duration = (max_frame - min_frame)/10 + 1, end_pause = 10)
  anim_save(paste("Optimal Path/Sample Plays/", game, "_", play, "_", rteam, "vs", kteam, "_path.mp4", sep = ""), animation2)
  toc()
  
}



for (i in 1:nrow(punt_df_v7)) {
  
  hull_animation = punt_df_v7$hull_animation[[i]]
  game = punt_df_v7$gameId[[i]]
  play = punt_df_v7$playId[[i]]
  rteam = punt_df_v7$rteamname[[i]]
  kteam = punt_df_v7$kteamname[[i]]
  max_frame = max(punt_df_v7$player_locations[[i]]$frameId)
  min_frame = min(punt_df_v7$player_locations[[i]]$frameId)
  
  print(paste("Saving ", game, "_", play, " ", rteam, " vs ", kteam, " (", i, " of ", nrow(punt_df_v7), ")", sep = ""))
  
  # # Save gif
  # tic()
  # anim_save(paste("Optimal Path/Sample Plays/", game, "_", play, "_", rteam, "vs", kteam, "_hull.gif", sep = ""), hull_animation,
  #           fps = 10, duration = (max_frame - min_frame)/10 + 1, end_pause = 10)
  # toc()
  
  # Save mp4
  tic()
  animation2 = animate(hull_animation, renderer = ffmpeg_renderer(), fps = 10, duration = (max_frame - min_frame)/10 + 1, end_pause = 10)
  anim_save(paste("Optimal Path/Sample Plays/", game, "_", play, "_", rteam, "vs", kteam, "_hull.mp4", sep = ""), animation2)
  toc()
  
}







