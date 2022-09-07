
# Functions used in full_pipeline.R

##################################
#### STEP 0: DATA PREPARATION ####
##################################

# Clean up the punt data for each frame and identify player types (ie returner, return team, kicking team)
clean_punt_data = function(df) {
  
  library(dplyr)
  
  df_output = df %>% 
    # Identify carrier (returner), blockers (returning team), and defenders (kicking team)
    # Calculate distance to ball so we can identify carrier
    mutate(ball_x = .$x_std[.$displayName == "football"],
           ball_y = .$y_std[.$displayName == "football"],
           dist_to_ball = sqrt((x_std - ball_x)^2 + (y_std - ball_y)^2)) %>%
    # Identify player type
    mutate(player_type = case_when(
      displayName == "football" ~ "Football",
      returnerId == nflId ~ "Returner",
      #dist_to_ball == min(.$dist_to_ball[.$displayName != "football"]) ~ "Returner",
      (team == "home" & home_possesion == FALSE) | (team == "away" & away_possesion == FALSE) ~ "Returning Team",
      TRUE ~ "Kicking Team"
    )) %>%
    # Create player codes (team + jersey number)
    mutate(type_code = case_when(
      player_type == "Football" ~ "F",
      player_type == "Returner" ~ "PR",
      player_type == "Returning Team" ~ "RT",
      player_type == "Kicking Team" ~ "KT"
    )) %>%
    mutate(jerseyNumber = ifelse(is.na(jerseyNumber), "", jerseyNumber)) %>%
    mutate(player_code = paste(type_code, jerseyNumber, sep = "")) %>%
    # Adjust xy-coordinates and directionality
    mutate(x_coor = 53.33 - y_std, y_coor = x_std, dir = 270 - dir_std) %>%
    # Select columns and arrange row order
    select(nflId, player_code, player_type, jerseyNumber, displayName, x_coor, y_coor, dir, s) %>%
    arrange(player_type)
  
  return(df_output)
}

#################################################
#### STEP 1: CREATE WINDOWS AND ACTION ZONES ####
#################################################

# Create windows between defenders that the carrier can move through
create_windows = function(defenders, carrier) {
  
  # Set carrier information
  carrier_code = carrier$player_code[1]
  carrier_x = carrier$x_coor[1]
  carrier_y = carrier$y_coor[1]
  
  if (nrow(defenders) >= 3) {
    
    ## GET WINDOWS WITHIN CONVEX HULL CREATED BY DEFENDERS ##
    
    # Run Delaunay triangulation
    delaunay = tri.mesh(x = defenders$x_coor, y = defenders$y_coor)
    
    # Get all connecting pairs from Delaunay
    neighbours = neighbours(delaunay)
    
    # Create data frame of 'windows' in the Delaunay triangulation
    delaunay_windows = neighbours %>%
      melt() %>%
      select(id1 = L1, id2 = value) %>%
      mutate(player1 = defenders$player_code[id1], x1 = defenders$x_coor[id1], y1 = defenders$y_coor[id1], 
             player2 = defenders$player_code[id2], x2 = defenders$x_coor[id2], y2 = defenders$y_coor[id2]) %>%
      rowwise() %>%
      mutate(key = paste(sort(c(player1, player2)), collapse="")) %>%
      distinct(key, .keep_all=T) %>%
      select(-key, -id1, -id2) %>%
      mutate(window_type = "Triangle")
    
    ## GET WINDOWS OUTSIDE OF CONVEX HULL ##
    
    # Get index of defenders that make up the convex hull
    hull_index = chull(defenders$x_coor, defenders$y_coor)
    
    # Create a polygon object 
    polygon = defenders[hull_index,] %>% select(x_coor, y_coor)
    
    # Create a data frame of defenders in the hull
    defender_hull = defenders[hull_index,] %>% 
      select(player_code, x_coor, y_coor) %>%
      mutate(is_left_side = map2(x_coor, y_coor, ~point.in.polygon(.x - 0.001, .y, polygon$x_coor, polygon$y_coor) == 0) %>% as.numeric()) %>%
      mutate(is_right_side = map2(x_coor, y_coor, ~point.in.polygon(.x + 0.001, .y, polygon$x_coor, polygon$y_coor) == 0) %>% as.numeric()) %>%
      mutate(is_on_top = map2(x_coor, y_coor, ~point.in.polygon(.x, .y + 0.001, polygon$x_coor, polygon$y_coor) == 0) %>% as.numeric()) %>%
      mutate(is_on_bottom = map2(x_coor, y_coor, ~point.in.polygon(.x, .y - 0.001, polygon$x_coor, polygon$y_coor) == 0) %>% as.numeric())
    
    # Get left side of the hull and identify which points should have forward or left windows
    left_hull = defender_hull %>%
      filter(is_left_side == 1) %>%
      arrange(desc(y_coor)) %>%
      mutate(slope_above = (lag(y_coor) - y_coor)/(lag(x_coor) - x_coor)) %>%
      mutate(slope_below = (lead(y_coor) - y_coor)/(lead(x_coor) - x_coor)) %>%
      mutate(slope_above = ifelse(is.na(slope_above) & is_on_top, 0.5, slope_above)) %>%
      mutate(slope_below = ifelse(is.na(slope_below) & is_on_bottom, 100, slope_below)) %>%
      mutate(turning_point = ifelse((slope_above < 1 & slope_above > 0) & (slope_below < 0 | slope_below > 1), 1, 0)) %>%
      mutate(side = cumsum(turning_point)) %>%
      arrange(y_coor) %>%
      mutate(forward = cumsum(turning_point))
    
    # Get right side of the hull and identify which points should have forward or right windows
    right_hull = defender_hull %>%
      filter(is_right_side == 1) %>%
      arrange(desc(y_coor)) %>%
      mutate(slope_above = (lag(y_coor) - y_coor)/(lag(x_coor) - x_coor)) %>%
      mutate(slope_below = (lead(y_coor) - y_coor)/(lead(x_coor) - x_coor)) %>%
      mutate(slope_above = ifelse(is.na(slope_above) & is_on_top, -0.5, slope_above)) %>%
      mutate(slope_below = ifelse(is.na(slope_below) & is_on_bottom, -100, slope_below)) %>%
      mutate(turning_point = ifelse((slope_above < 0 & slope_above > -1) & (slope_below < -1 | slope_below > 0), 1, 0)) %>%
      mutate(side = cumsum(turning_point)) %>%
      arrange(y_coor) %>%
      mutate(forward = cumsum(turning_point))
    
    # Get forward windows
    forward_windows = bind_rows(left_hull, right_hull) %>%
      filter(forward >= 1) %>%
      rowwise() %>%
      mutate(key = paste(sort(c(x_coor, y_coor)), collapse="")) %>%
      distinct(key, .keep_all=T) %>%
      select(-key) %>%
      ungroup() %>%
      select(player1 = player_code, x1 = x_coor, y1 = y_coor) %>%
      mutate(player2 = NA_character_, x2 = x1, y2 = carrier_y, window_type = "Forward Extension")
    
    # Get side windows
    side_windows = bind_rows(left_hull %>% filter(side >= 1) %>% mutate(side = "Left"), 
                             right_hull %>% filter(side >= 1) %>% mutate(side = "Right")) %>%
      select(player1 = player_code, x1 = x_coor, y1 = y_coor, side) %>%
      mutate(player2 = NA_character_, 
             x2 = ifelse(side == "Left", 0, 53.33), 
             y2 = y1, 
             window_type = ifelse(side == "Left", "Left Extension", "Right Extension")) %>%
      select(-side)
    
    # Get sideline windows
    sideline_windows = side_windows %>%
      group_by(window_type) %>%
      arrange(desc(y2)) %>%
      mutate(x1 = x2, y1 = lag(y2)) %>%
      mutate(y1 = ifelse(is.na(y1), carrier_y, y1)) %>%
      ungroup() %>%
      mutate(window_type = str_replace(window_type, "Extension", "Sideline")) %>%
      mutate(player1 = NA_character_) %>%
      select(window_type, everything())
    
    ## COMBINE ALL AND GET ACTION ZONE ##
    windows = bind_rows(delaunay_windows, forward_windows, side_windows, sideline_windows) %>%
      mutate(carrier = carrier_code, carrier_x = carrier_x, carrier_y = carrier_y) %>%
      # Get point of intersection
      mutate(yint = pmap(list(x1, y1, x2, y2, carrier_x), ~ifelse(..1 == ..3, NA, approx(x = c(..1,..3), y = c(..2,..4), xout = ..5)$y))) %>%
      unnest(yint) %>%
      # Get the vertical distance between the window and the carrier
      mutate(ydist = carrier_y - yint) %>%
      # Window with minimum distance is in action zone
      mutate(is_actionable = ifelse(ydist == min(.$ydist, na.rm = TRUE), 1, 0)) %>%
      # Get windows on the side of the action zone
      mutate(xdist = carrier_x - x1) %>%
      mutate(xabs = xdist > 0) %>%
      mutate(group = ifelse(y1 == carrier_y | y2 == carrier_y, "close", "other")) %>%
      group_by(group, xabs) %>%
      mutate(min = min(abs(xdist))) %>%
      mutate(is_actionable = ifelse(group == "close" & min == abs(xdist), 1, is_actionable)) %>%
      mutate(is_actionable = ifelse(is.na(is_actionable), 0, is_actionable)) %>%
      ungroup() %>%
      mutate(is_sideline = ifelse(grepl("Sideline", window_type), 2, 1)) %>%
      arrange(is_sideline, desc(min(y1, y2))) %>%
      mutate(window_id = ifelse(as.numeric(rownames(.)) < 10, paste("W0", rownames(.), sep = ""), paste("W", rownames(.), sep = ""))) %>%
      select(window_id, player1, x1, y1, player2, x2, y2, carrier, carrier_x, carrier_y, window_type, is_actionable)
    
  } else if (nrow(defenders) == 2) {
    
    # Create windows data frame
    windows = data.frame(
      window_id = c("W06", "W04", "W02", "W01", "W03", "W05", "W07"),
      player1 = c(NA, NA, (defenders %>% arrange(x_coor))$player_code[1], (defenders %>% arrange(x_coor))$player_code[1], 
                  (defenders %>% arrange(x_coor))$player_code[2], (defenders %>% arrange(x_coor))$player_code[2], NA),
      x1 = c(0, 0, (defenders %>% arrange(x_coor))$x_coor[1], (defenders %>% arrange(x_coor))$x_coor[1], 
             (defenders %>% arrange(x_coor))$x_coor[2], (defenders %>% arrange(x_coor))$x_coor[2], 53.33),
      y1 = c(carrier$y_coor, (defenders %>% arrange(x_coor))$y_coor[1], (defenders %>% arrange(x_coor))$y_coor[1], 
             (defenders %>% arrange(x_coor))$y_coor[1], (defenders %>% arrange(x_coor))$y_coor[2], 
             (defenders %>% arrange(x_coor))$y_coor[2], (defenders %>% arrange(x_coor))$y_coor[2]),
      player2 = c(NA, (defenders %>% arrange(x_coor))$player_code[1], (defenders %>% arrange(x_coor))$player_code[1], 
                  (defenders %>% arrange(x_coor))$player_code[2], (defenders %>% arrange(x_coor))$player_code[2], NA, NA),
      x2 = c(0, (defenders %>% arrange(x_coor))$x_coor[1], (defenders %>% arrange(x_coor))$x_coor[1], 
             (defenders %>% arrange(x_coor))$x_coor[2], (defenders %>% arrange(x_coor))$x_coor[2], 53.33, 53.33),
      y2 = c((defenders %>% arrange(x_coor))$y_coor[1], (defenders %>% arrange(x_coor))$y_coor[1], 
             carrier$y_coor, (defenders %>% arrange(x_coor))$y_coor[2], carrier$y_coor, 
             (defenders %>% arrange(x_coor))$y_coor[2], carrier$y_coor),
      carrier = carrier_code, carrier_x = carrier_x, carrier_y = carrier_y,
      window_type = c("Left Sideline", "Left Extension", "Forward Extension", "Players", "Forward Extension", "Right Extension", "Right Sideline"),
      is_actionable = c(ifelse(carrier$x_coor < (defenders %>% arrange(x_coor))$x_coor[1], 1, 0),
                        ifelse(carrier$x_coor < (defenders %>% arrange(x_coor))$x_coor[1], 1, 0),
                        ifelse(carrier$x_coor < (defenders %>% arrange(x_coor))$x_coor[2], 1, 0),
                        ifelse(carrier$x_coor < (defenders %>% arrange(x_coor))$x_coor[2] & carrier$x_coor >= (defenders %>% arrange(x_coor))$x_coor[1], 1, 0),
                        ifelse(carrier$x_coor >= (defenders %>% arrange(x_coor))$x_coor[1], 1, 0),
                        ifelse(carrier$x_coor >= (defenders %>% arrange(x_coor))$x_coor[2], 1, 0),
                        ifelse(carrier$x_coor >= (defenders %>% arrange(x_coor))$x_coor[2], 1, 0))
    )
    
  } else if (nrow(defenders) == 1) {
    
    # Create windows data frame
    windows = data.frame(
      window_id = c("W04", "W02", "W01", "W03", "W05"),
      player1 = c(NA, NA, defenders$player_code[1], defenders$player_code[1], NA),
      x1 = c(0, 0, defenders$x_coor[1], defenders$x_coor[1], 53.33),
      y1 = c(carrier$y_coor, defenders$y_coor[1], defenders$y_coor[1], defenders$y_coor[1], defenders$y_coor[1]),
      player2 = c(NA, defenders$player_code[1], defenders$player_code[1], NA, NA),
      x2 = c(0, defenders$x_coor[1], defenders$x_coor[1], 53.33, 53.33),
      y2 = c(defenders$y_coor[1], defenders$y_coor[1], carrier$y_coor, defenders$y_coor[1], carrier$y_coor),
      carrier = carrier_code, carrier_x = carrier_x, carrier_y = carrier_y,
      window_type = c("Left Sideline", "Left Extension", "Forward Extension", "Right Extension", "Right Sideline"),
      is_actionable = c(ifelse(carrier$x_coor < defenders$x_coor[1], 1, 0),
                        ifelse(carrier$x_coor < defenders$x_coor[1], 1, 0),
                        1,
                        ifelse(carrier$x_coor >= defenders$x_coor[1], 1, 0),
                        ifelse(carrier$x_coor >= defenders$x_coor[1], 1, 0))
    )
    
  } else {
    
    # Create windows data frame
    windows = data.frame(
      player1 = NA, x1 = NA, y1 = NA, player2 = NA, x2 = NA, y2 = NA, 
      carrier = NA, carrier_x = NA, carrier_y = NA, window_type = NA,
      is_actionable = NA
    )
  }
  return(windows)
}



########################################################
#### STEP 2: CALCULATE FIELD PRESSURE ALONG WINDOWS ####
########################################################

# Calculate minimum defender arrival time to target location(s) on the field
arrival_time = function(target_x, target_y,
                        defenders_x, defenders_y, speed, dir,
                        blockers_x, blockers_y,
                        reaction_time = 0, max_speed = 7, blocker_time_multiplier = 5) {
  
  # i. Determine defenders' positions after reaction_time total seconds at their same speed and direction
  angle = case_when(
    dir <= 90 ~ 90 - dir,
    dir > 90 & dir < 180 ~ dir - 90,
    dir > 180 & dir < 270 ~ 270 - dir,
    TRUE ~ dir - 270
  )
  
  reaction_x = defenders_x + ifelse(dir < 180, cos(angle * pi/180)*(speed * reaction_time), -cos(angle * pi / 180)*(speed * reaction_time))
  reaction_y = defenders_y + ifelse(dir > 90 & dir < 270, sin(angle * pi/180)*(speed * reaction_time), -sin((angle * pi)/180)*(speed * reaction_time))
  
  # ii. Calculate amount of time to penalize each defender due to blockers in between them and the target location
  # The idea here is to:
  #   1. Create a straight line between the defender and the target location
  #   2. Find the perpendicular projection of each blocker onto the line from Step 1
  #   3. If this projection does not lie in between the defender and the target, then penalty = 0
  #      Else use a Gaussian kernel with StdDev = the distance between the defender and the blocker's 
  #      perpendicular projection and x = the blocker's distance away from the perpendicular projection 
  #      to obtain the time penalty for the defender based on the blocker's position (multiplied by the)
  #      max_time_penalty parameter
  
  # Initialize vector to store minimum times it takes a defender to reach each target
  min_times = c()
  
  # For each target location calculate the minimum time it will take a defender to reach it
  for (k in 1:length(target_x)) {
    
    # Initialize vector to store penalty times for each defender
    penalty_time = c()
    
    # For each defender calculate the total time penalty he incurs due to blockers
    for (i in 1:length(defenders_x)) {
      
      # Set total penalty time = 0
      total_penalty = 0
      
      # For each blocker calculate the time penalty he imposes on the defender
      for (j in 1:length(blockers_x)) {
        
        # Set (x1,y1) = location of defender after reaction time, (x2,y2) = target location, (x3,y3) = blocker location
        x1 = reaction_x[i]; y1 = reaction_y[i]
        x2 = target_x[k]; y2 = target_y[k]
        x3 = blockers_x[j]; y3 = blockers_y[j]
        
        # Calculate the perpendicular projection of the blocker's position onto the line formed by the defender and the target
        b = -((x1-x3)*(x2-x1)+(y1-y3)*(y2-y1))/((x2-x1)^2+(y2-y1)^2)
        intercept_x = x1 + b*(x2 - x1)
        intercept_y = y1 + b*(y2 - y1)
        
        # Calculate various distances
        dist_to_intercept = sqrt((intercept_y - y1)^2 + (intercept_x - x1)^2)
        dist_to_target = sqrt((y2 - y1)^2 + (x2 - x1)^2)
        intercept_to_blocker = sqrt((intercept_y - y3)^2 + (intercept_x - x3)^2)
        target_to_intercept = sqrt((y2 - intercept_y)^2 + (x2 - intercept_x)^2)
        
        # Calculate penalty time
        x = intercept_to_blocker
        sd = dist_to_intercept
        penalty = ifelse((dist_to_intercept >= dist_to_target) | (target_to_intercept >= dist_to_target),
                         0,
                         blocker_time_multiplier * (1 / ((sd) * sqrt(2*pi))) * exp(-(1/2) * (x/(sd))^2))
        
        # Add penalty time for this blocker to the defender's total penalty time
        total_penalty = total_penalty + penalty
        
        
      }
      
      # Add the defenders total penalty time into the penalty_time vector
      penalty_time = c(penalty_time, total_penalty)
      
    }
    
    # Calculate total time to target for each defender
    time_total = reaction_time + penalty_time + sqrt((reaction_x - target_x[k])^2 + (reaction_y - target_y[k])^2) / max_speed
    
    # Add minimum time to target to vector of minimum times for all targets
    min_times = c(min_times, min(time_total))
    
  }
  
  return(min_times)
  
}



# Calculate arrival time along windows and find point of maximum arrival time (ie least pressured)
find_arrival_time = function(windows, player_locations) {
  
  # Extract data required from player_locations to calculate arrival time
  defenders_x = (player_locations %>% filter(player_type == "Kicking Team"))$x_coor
  defenders_y = (player_locations %>% filter(player_type == "Kicking Team"))$y_coor 
  defenders_speed = (player_locations %>% filter(player_type == "Kicking Team"))$s
  defenders_dir = (player_locations %>% filter(player_type == "Kicking Team"))$dir
  blockers_x = (player_locations %>% filter(player_type == "Returning Team"))$x_coor
  blockers_y = (player_locations %>% filter(player_type == "Returning Team"))$y_coor
  
  
  windows = windows %>%
    # Get 25 points along each window and calculate arrival times
    mutate(window_pts = pmap(list(x1, x2, y1, y2), ~suppressWarnings(pointsAtEvenSpacing(matrix(c(..1, ..2, ..3, ..4), 2, 2), n = 27))[2:26,] %>%
                               as.data.frame() %>%
                               rename(x = V1, y = V2) %>%
                               mutate(midpt = c(rep(0,12),1,rep(0,12))) %>%
                               mutate(euclid1 = sqrt((x-..1)^2 + (y-..3)^2)) %>%
                               mutate(euclid2 = sqrt((x-..2)^2 + (y-..4)^2)) %>%
                               filter(!((midpt == 0) & (euclid1 < 1.5 | euclid2 < 1.5))) %>%
                               select(-euclid1, -euclid2) %>%
                               mutate(arrival_time = arrival_time(target_x = x, target_y = y,
                                                                  defenders_x = defenders_x, defenders_y = defenders_y,
                                                                  speed = defenders_speed, dir = defenders_dir,
                                                                  blockers_x = blockers_x, blockers_y = blockers_y,
                                                                  reaction_time = 0, max_speed = 7, blocker_time_multiplier = 2))
    )) %>%
    # Calculate point of maximum arrival time for each window
    mutate(min_pt = map(window_pts, ~.x %>%
                          slice(which.max(arrival_time)) %>%
                          rename(safest_x = x, safest_y = y, max_arrival_time = arrival_time) %>%
                          select(-midpt)
    )) %>%
    unnest(min_pt)
  
  return(windows)
  
}



# Calculate minimum arrival time to carrier
get_arrival_to_carrier = function(player_locations) {
  
  # Extract data required from player_locations to calculate arrival time
  defenders_x = (player_locations %>% filter(player_type == "Kicking Team"))$x_coor
  defenders_y = (player_locations %>% filter(player_type == "Kicking Team"))$y_coor 
  defenders_speed = (player_locations %>% filter(player_type == "Kicking Team"))$s
  defenders_dir = (player_locations %>% filter(player_type == "Kicking Team"))$dir
  blockers_x = (player_locations %>% filter(player_type == "Returning Team"))$x_coor
  blockers_y = (player_locations %>% filter(player_type == "Returning Team"))$y_coor
  target_x = (player_locations %>% filter(player_type == "Returner"))$x_coor[1]
  target_y = (player_locations %>% filter(player_type == "Returner"))$y_coor[1]
  
  pressure = arrival_time(target_x, target_y,
                          defenders_x, defenders_y, defenders_speed, defenders_dir,
                          blockers_x, blockers_y,
                          reaction_time = 0, max_speed = 7, blocker_time_multiplier = 5)
  
  return(pressure)
  
}



#############################
#### SHINY APP FUNCTIONS ####
#############################

## FINDING OPTIMAL PATH ##

# Find optimal path via A* algorithm
astar_dynamic_nodes = function(window_pairs, alpha, carrier_speed = 9, weight_multiplier = 10){#}, updateProgress = NULL) {
  
  ## STEP 1: INITIALIZE PAIRS AND PATHS_TAKEN DATA FRAMES ##
  
  # Create the pairs data frame (this stores one row per connecting window pairs)
  pairs = window_pairs %>% 
    filter(transition == 1) %>% 
    filter(window2 != "carrier") %>%
    filter(window1 != "clear") %>%
    select(window1, window2, outcome_coords)
  
  # Create the paths_taken data frame (this stores each possible optimal path we find as we move through the graph)
  paths_taken = window_pairs %>%
    filter(window1 == "carrier") %>%
    head(1) %>%
    select(window_id = window1, x = x1, y = y1) %>%
    mutate(path = "", heuristic = y-10, distance = 0, 
           priority = heuristic + distance, visited = 0)
  
  
  ## STEP 2: ITERATE THROUGH WINDOWS ##
  
  repeat {
    
    ## STEP 2A: FIND WINDOW WITH LOWEST PRIORITY VALUE WHICH HAS NOT BEEN VISITED AND FILTER FOR ALL PATHS WITH THAT WINDOW ##
    lowest_priority = paths_taken %>% 
      filter(visited == 0) %>% 
      mutate(global_min = min(priority)) %>%
      group_by(window_id) %>%
      filter(any(priority == global_min)) %>%
      ungroup() %>% select(-global_min)
    
    lowest_priority_id = lowest_priority$window_id[1]
    
    
    ## STEP 2B: STOP IF ALL WINDOWS HAVE BEEN CHECKED OR WE REACH THE GOAL WINDOW ##
    if (nrow(lowest_priority) == 0) {
      return(NULL) # Return null if we have cycled through all nodes
    } else if (lowest_priority$window_id[1] == "clear") {
      
      # Obtain the path taken
      path = strsplit(lowest_priority$path, ";")[[1]]
      
      # Retrieve the optimal coordinates along this path
      final_path = data.frame()
      for (i in 1:(length(path)-1)) {
        window_temp = path[i+1]
        path_temp = paste(path[1:i], collapse = ";")
        final_path = bind_rows(final_path, paths_taken %>% filter(window_id == window_temp & path == path_temp))
      }
      
      # Rearrange data frame to make it ggplot friendly for us
      final_path = final_path %>%
        rename(window1 = window_id, x1 = x, y1 = y) %>%
        mutate(window2 = lead(window1), x2 = lead(x1), y2 = lead(y1)) %>%
        mutate(window2 = ifelse(is.na(window2), "clear", window2),
               x2 = ifelse(is.na(x2), x1, x2),
               y2 = ifelse(is.na(y2), y1 - 5, y2)) %>%
        select(window1, x1, y1, window2, x2, y2, everything())
      
      return(final_path)
      
    }
 
    ## STEP 2C: FIND THE OPTIMAL POINT ALONG EACH WINDOW THAT CONNECT TO THE LOWEST PRIORITY WINDOW ##
    best_options = pairs %>%
      # Find all possible options to move from the current location
      filter(window1 == lowest_priority_id) %>%
      left_join(lowest_priority %>% rename(window1 = window_id, x1 = x, y1 = y), by = "window1") %>%
      unnest(outcome_coords) %>% ungroup() %>%
      rename(x2 = x, y2 = y) %>%
      # Calculate the weights on each option
      mutate(carrier_time = sqrt((x2-x1)^2 + (y2-y1)^2) / carrier_speed) %>%
      mutate(prob_open = 1 / (1 + exp(-(arrival_time - carrier_time)))) %>%
      mutate(delta_y = y1-y2, delta_x = abs(x2-x1)) %>%
      mutate(fwd_movement = 0.4 + 0.2 * delta_y / (delta_y + delta_x)) %>%
      mutate(euclid = sqrt((x1-x2)^2 + (y1-y2)^2)) %>%
      mutate(scaled_arrival = 1 - (arrival_time - min(arrival_time))/(max(arrival_time) - min(arrival_time))) %>% 
      mutate(weight = ifelse(delta_y <= 0, 1000, weight_multiplier * (1-prob_open) * (1-fwd_movement))) %>%
      mutate(weight = ifelse(delta_y <= 0, 1000, (2 * (1 - alpha) * scaled_arrival + alpha) * euclid)) %>%
      # Update path, heuristic, distance and priority for each choice
      mutate(path = paste(path, window1, sep = ";")) %>%
      mutate(heuristic = y2-10, distance = distance + weight) %>%
      mutate(priority = distance + heuristic) %>%
      mutate(visited = 0) %>%
      select(window_id = window2, x = x2, y = y2, path, heuristic, distance, priority, visited) %>%
      # Select best choice for each connecting window
      group_by(window_id) %>%
      slice(which.min(priority)) %>%
      ungroup()
    
    
    ## STEP 2D: ADD THE NEW PATHS FORMED TO THE paths_taken DATA FRAME
    paths_taken = bind_rows(paths_taken, best_options) %>%
      # Determine if the connecting windows were visited already
      group_by(window_id) %>% mutate(visited = ifelse(sum(visited) > 0, 1, 0)) %>% ungroup()
    
    ## STEP 2E: SET THE LOWEST PRIORITY NODE WE JUST LOOKED AT AS VISITED ##
    paths_taken = paths_taken %>% mutate(visited = ifelse(window_id == lowest_priority_id, 1, visited))
    
  }
  
}



# Take in windows data and output the optimal path via the A* algorithm
find_optimal_path = function(windows, alpha, updateProgress = NULL) {
  
  # Extract carrier coordinates
  carrier_x = windows$carrier_x[1]
  carrier_y = windows$carrier_y[1]
  
  
  ## FIND ALL PAIRS OF WINDOWS THAT ARE DIRECTLY CONNECTED ##

  # Create data frame of windows + the current carrier position and the position beyond
  windows_plus = windows %>%
    add_row(data.frame(window_id = "carrier", safest_x = carrier_x, safest_y = carrier_y, window_type = "End")) %>%
    add_row(data.frame(window_id = "clear", safest_x = 53.33/2, safest_y = min(.$safest_y) - 30, window_type = "End")) %>%
    mutate(window_pts = map2(window_pts, window_id, ~if (.y == "clear") {data.frame(x = 53.33/2, y = 10, arrival_time = 10, midpt = 1)} else {.x}))

  # Set up window data to be joined to window_pairs below
  windows_join = windows %>%
    mutate(dummy = "a") %>%
    select(dummy, window_id, wx1 = x1, wy1 = y1, wx2 = x2, wy2 = y2) %>%
    mutate(slope_w = (wy2 - wy1)/(wx2 - wx1), intercept_w = wy2 - slope_w * wx2)

  # Find all connecting pairs of windows (no windows lie in between them)
  window_pairs = expand.grid(window1 = windows_plus$window_id, window2 = windows_plus$window_id) %>%
    filter(window1 != window2) %>%
    left_join(windows_plus %>% select(window1 = window_id, x1 = safest_x, y1 = safest_y), by = "window1") %>%
    left_join(windows_plus %>% select(window2 = window_id, x2 = safest_x, y2 = safest_y, outcome_coords = window_pts), by = "window2") %>%
    ungroup() %>%
    mutate(slope = (y2 - y1)/(x2 - x1), intercept = y2 - slope * x2) %>%
    mutate(dummy = "a") %>% left_join(windows_join, by = "dummy") %>%
    ungroup() %>%
    filter(window1 != window_id & window2 != window_id) %>%
    mutate(intersect_x = case_when(
      slope_w %in% c(Inf, -Inf) ~ wx1,
      slope %in% c(Inf, -Inf) ~ x1,
      TRUE ~ (intercept - intercept_w)/(slope_w - slope)
    )) %>%
    mutate(intersect_y = case_when(
      slope_w %in% c(Inf, -Inf) ~ slope * intersect_x + intercept,
      slope %in% c(Inf, -Inf) ~ slope_w * intersect_x + intercept_w,
      TRUE ~ slope * intersect_x + intercept
    )) %>%
    mutate(int_x = round(intersect_x,2), int_y = round(intersect_y,2)) %>%
    mutate_at(vars(c(x1,y1,x2,y2,wx1,wy1,wx2,wy2)), ~round(., digits = 2)) %>%
    select(-slope, -slope_w, -intercept, -intercept_w, -intersect_x, -intersect_y) %>%
    mutate(is_intersect = ((int_x <= x1 & int_x >= x2) | (int_x <= x2 & int_x >= x1)) & 
             ((int_y <= y1 & int_y >= y2) | (int_y <= y2 & int_y >= y1)) & 
             ((int_x <= wx1 & int_x >= wx2) | (int_x <= wx2 & int_x >= wx1)) & 
             ((int_y <= wy1 & int_y >= wy2) | (int_y <= wy2 & int_y >= wy1))) %>%
    mutate(is_intersect = ifelse(is.na(is_intersect), FALSE, is_intersect)) %>%
    group_by(window1, window2, x1, y1, outcome_coords) %>%
    summarize(sum = sum(is_intersect)) %>%
    ungroup() %>%
    mutate(transition = ifelse(sum == 0, 1, 0))
  
  if (is.function(updateProgress)) {
    updateProgress(detail = "A* Algorithm")
  }
  ## RUN A* ALGORITHM THAT FINDS OPTIMAL COORDINATES AT NEXT WINDOWS ##
  optimal_coords = astar_dynamic_nodes(window_pairs, alpha = alpha)#, updateProgress)
  
  return(optimal_coords)
  
}



## FINDING FRECHET DISTANCE ##

# Find the frechet distance between the carrier path and the optimal path over the next three seconds
find_path_frechet = function(carrier_path, optimal_path, frame, sec = 3) {
  
  curr_y = carrier_path %>% filter(frameId == frame) %>% select(y_coor) %>% unlist() %>% unname()
  
  carrier_path = carrier_path %>% filter(frameId >= frame & y_coor >= curr_y - 5)
  
  if (nrow(optimal_path) == 0 | nrow(carrier_path) == 0) {
    return(NA_real_)
  } else if (min(carrier_path$y_coor) >= curr_y - 1 | min(optimal_path$y1) >= curr_y - 1) {
    return(NA_real_)
  } else {
    
    cpath = carrier_path %>%
      select(frameId, x = x_coor, y = y_coor) %>%
      filter(y >= curr_y - 10)
    
    opath = optimal_path %>%
      rowwise() %>%
      filter(between(y1, min(cpath$y), Inf)) %>%
      ungroup() %>%
      mutate(points = pmap(list(x1,x2,y1,y2), ~approx(y=c(..1,..2), x=c(..3,..4), xout = cpath$y[which(cpath$y >= ..4 & cpath$y <= ..3)]))) %>%
      mutate(points = map(points, ~data.frame(x = .x$y, y = .x$x))) %>%
      select(points) %>%
      unnest(points) %>% ungroup()
    
    frechet = distFrechet(Px = cpath$y, Py = cpath$x,
                          Qx = opath$y, Qy = opath$x, 
                          FrechetSumOrMax = "max", timeScale = 1)
    
    frechet = frechet * 5/ (curr_y - min(c(cpath$y, opath$y)))
    
    return(frechet)
  }
  
}


# Find the frechet distance between the carrier path and the optimal path over the next three seconds
find_path_frechet_v2 = function(carrier_path, optimal_path, frame, sec = 3) {
  
  # Get carrier path over next 5 yards
  cpath = carrier_path %>% 
    filter(frameId >= frame) %>%
    mutate(euclid = sqrt((y_coor - lag(y_coor))^2 + (x_coor - lag(x_coor))^2)) %>%
    mutate(euclid = ifelse(is.na(euclid), 0, euclid)) %>%
    mutate(c_euclid = cumsum(euclid)) %>%
    filter(c_euclid <= 5) %>%
    select(-euclid) %>%
    rename(x = x_coor, y = y_coor)
  
  # Get optimal path over next 5 yards
  opath = optimal_path %>%
    mutate(pts = pmap(list(x1, x2, y1, y2), ~suppressWarnings(pointsAtEvenSpacing(matrix(c(..1, ..2, ..3, ..4), 2, 2), n = 30))[1:29,] %>%
                        as.data.frame() %>%
                        rename(x = V1, y = V2)
    )) %>%
    select(window1, window2, pts) %>%
    unnest(pts) %>% ungroup() %>%
    mutate(euclid = sqrt((y - lag(y))^2 + (x - lag(x))^2)) %>%
    mutate(euclid = ifelse(is.na(euclid), 0, euclid)) %>%
    mutate(c_euclid = cumsum(euclid)) %>%
    filter(c_euclid <= 5) %>%
    select(-euclid)
  
  if ((max(cpath$c_euclid) < 1) | (max(opath$c_euclid) < 1)) {
    return(NA_real_)
  } else {
    
    Rc = max(cpath$c_euclid)
    Ro = max(opath$c_euclid)
    R = min(Rc, Ro)
    
    if (Rc < Ro) {
      opath = opath %>% filter(c_euclid <= Rc + 0.3)
    } else {
      cpath = cpath %>% filter(c_euclid <= Ro + 0.3)
    }
    
    frechet = distFrechet(Px = cpath$y, Py = cpath$x,
                          Qx = opath$y, Qy = opath$x, 
                          FrechetSumOrMax = "max", timeScale = 1)
    
    frechet = frechet * 5 / R
    
    return(frechet)
    
  }
}



# Smooth out A* paths by finding the Frechet distance between the current, previous and next frames
smooth_path = function(current, previous, next_up) {
  
  if (is.null(previous) | is.null(next_up)) {
    return(0)
  } else {
    # Calculate Frechet distance between path in previous frame and path in current frame
    prev_curr = distFrechet(Px = current$y1, Py = current$x1,
                            Qx = previous$y1, Qy = previous$x1, 
                            FrechetSumOrMax = "max", timeScale = 1)
    # Calculate Frechet distance between path in next frame and path in current frame
    curr_next = distFrechet(Px = current$y1, Py = current$x1,
                            Qx = next_up$y1, Qy = next_up$x1, 
                            FrechetSumOrMax = "max", timeScale = 1)
    # Calculate Frechet distance between path in previous frame and path in next frame
    prev_next = distFrechet(Px = previous$y1, Py = previous$x1,
                            Qx = next_up$y1, Qy = next_up$x1, 
                            FrechetSumOrMax = "max", timeScale = 1)
    # If the Frechet distance between the pairings spikes for the current frame but is low between the previous and next frames then it requires smoothing
    cond_1 = (prev_curr / prev_next) > 2
    cond_2 = (curr_next / prev_next) > 2
    cond_3 = between(curr_next / prev_curr, 0.75, 1.5)
    cond_4 = prev_curr > 1 & curr_next > 1
    
    requires_smoothing = ifelse(all(cond_1, cond_2, cond_3, cond_4), 1, 0)
    
    return(requires_smoothing)
  }
  
}


# Create table to input in path plots and animations
create_path_table = function(frame, frechet, player_locations) {
  
  # Extract data required from player_locations to calculate arrival time
  defenders_x = (player_locations %>% dplyr::filter(player_type == "Kicking Team"))$x_coor
  defenders_y = (player_locations %>% dplyr::filter(player_type == "Kicking Team"))$y_coor 
  defenders_speed = (player_locations %>% dplyr::filter(player_type == "Kicking Team"))$s
  defenders_dir = (player_locations %>% dplyr::filter(player_type == "Kicking Team"))$dir
  blockers_x = (player_locations %>% dplyr::filter(player_type == "Returning Team"))$x_coor
  blockers_y = (player_locations %>% dplyr::filter(player_type == "Returning Team"))$y_coor
  target_x = (player_locations %>% dplyr::filter(player_type == "Returner"))$x_coor[1]
  target_y = (player_locations %>% dplyr::filter(player_type == "Returner"))$y_coor[1]
  
  pressure = arrival_time(target_x, target_y,
                          defenders_x, defenders_y, defenders_speed, defenders_dir,
                          blockers_x, blockers_y,
                          reaction_time = 0, max_speed = 7, blocker_time_multiplier = 5)
  
  my_tbl = data.frame("Metric" = c("Frame", "Defender Arrival Time to Returner", "Path Deviation"),
                      "Value" = c(frame, pressure, frechet))
  
  return(my_tbl)
  
}


## CREATING STATIC PLOTS ##

# Determine team colours
choose_colours = function(kicking_team, returning_team) {
  
  team_colours = c(colours$X2[colours$X1 == kicking_team], colours$X3[colours$X1 == returning_team], colours$X2[colours$X1 == returning_team])
  
  if (kicking_team == "DEN" & returning_team == "PIT") {
    team_colours[1] = "#CC5500"
  }
  
  if (kicking_team == "LAC" & returning_team == "NE") {
    team_colours[1] = "gold"
  }
  
  if (kicking_team == "ARI" & returning_team == "NE") {
    team_colours[2] = "blue"
  }
  
  if (kicking_team == "DEN" & returning_team == "LV") {
    team_colours[1] = "orange"
  }
  
  if (kicking_team == "TB" & returning_team == "KC") {
    team_colours[1] = "grey50"
  }
  
  if (kicking_team == "ATL" & returning_team == "KC") {
    team_colours[1] = "black"
  }
  
  return(team_colours)
}



# Create static ggplot of optimal path and windows
create_path_plot = function(carrier_y,
                            kicking_team,
                            returning_team,
                            player_locations,
                            windows,
                            field_pressure,
                            optimal_path,
                            carrier_path,
                            path_table) {
  
  # Set plot specs
  xmin = 0
  xmax = 53.33
  ymin = 0
  ymax = 120
  hash.right = 38.35
  hash.left = 12
  hash.width = 3.3
  field.alpha = 0.7
  
  # Set team colours
  team_colours = choose_colours(kicking_team, returning_team)
  
  # Create field
  hash_marks = expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110)) %>%
    filter(!(floor(y %% 5) == 0)) %>%
    filter(y < ymax, y > ymin)
  
  yard_lines = data.frame(
    x = xmin, 
    xend =  xmax,
    y = seq(max(10, ymin), min(ymax, 110), by = 5), 
    yend = seq(max(10, ymin), min(ymax, 110), by = 5)
  )
  
  yard_line_text = data.frame(
    y = seq(10, 110, by = 10), 
    x_left = rep(hash.left, 11), 
    label_left = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
    x_right = rep((xmax - hash.left), 11), 
    label_right = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   ")
  )
  
  field_outline = data.frame(
    x = c(xmin, xmin, xmax, xmax), 
    y = c(ymin, ymax, ymax, ymin), 
    xend = c(xmin, xmax, xmax, xmin), 
    yend = c(ymax, ymax, ymin, ymin)
  )
  
  # Add team names to player locations
  player_locations = player_locations %>%
    filter(player_code != "F") %>%
    mutate(player_type = case_when(
      player_type == "Kicking Team" ~ paste0("Kicking Team (", kicking_team, ")"),
      player_type != "Kicking Team" ~ paste0(player_type, " (", returning_team, ")")
    ))
  
  # Reformat path_table so that we can use geom_table
  path_table_nest = tibble(x = 80, y = -30, tb = list(
    path_table %>%
      mutate(Value = round(Value,2)) %>%
      mutate(Value = ifelse(is.na(Value), "", Value)) %>%
      row_to_names(1)
  ))
  
  # Create plot
  path_plot = ggplot() +
    ## CREATE FIELD ##
    # Set colour and size parameters
    scale_size_manual(values = c(6, 4, 6), guide = "none") + 
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = team_colours) + 
    scale_colour_gradientn(
      colours = c("red", "yellow", "forestgreen"),
      guide = guide_colorbar(barwidth = 0.8, barheight = 8)
    ) +
    # Add hash marks
    geom_text(data = hash_marks %>% filter(x < 55/2), aes(x = x, y = y - carrier_y), label = "_", hjust = 0, vjust = -0.2, alpha = field.alpha) +
    geom_text(data = hash_marks %>% filter(x > 55/2), aes(x = x, y = y - carrier_y), label = "_", hjust = 1, vjust = -0.2, alpha = field.alpha) +
    # Add yard lines
    geom_segment(data = yard_lines, aes(x = x, y = y - carrier_y, xend = xend, yend = yend - carrier_y), alpha = field.alpha) +
    # Add yard line text
    geom_text(data = yard_line_text, aes(x = x_left, y = y - carrier_y, label = label_left), angle = 270, size = 4, alpha = field.alpha) +
    geom_text(data = yard_line_text, aes(x = x_right, y = y - carrier_y, label = label_right), angle = 90, size = 4, alpha = field.alpha) +
    # Add field exterior
    geom_segment(data = field_outline, aes(x = x, xend = xend, y = y - carrier_y, yend = yend - carrier_y), alpha = field.alpha) +
    ## ADD IN PLAYERS AND WINDOWS
    # Add in static path of returner
    geom_path(data = carrier_path, aes(x = x_coor, y = y_coor - carrier_y), colour = "grey70", alpha = 0.8, size = 1.5) +
    # Add in horizontal line for returner
    geom_segment(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = -2, xend = 55.33, y = y_coor - carrier_y, yend = y_coor - carrier_y), linetype = "longdash", alpha = 0.7)
  
  if (!is.null(windows)) {
    path_plot = path_plot +
      # Add windows
      geom_segment(data = windows, aes(x = x1, xend = x2, y = y1 - carrier_y, yend = y2 - carrier_y), alpha = 0.4) +
      # Add in heat points of field pressure
      geom_path(data = field_pressure, aes(x = x, y = y - carrier_y, colour = arrival_time, group = window_id), size = 1.4, alpha = 0.6)
  }
  
  path_plot = path_plot +
    # Add players
    geom_point(data = player_locations, aes(x = x_coor, y = y_coor - carrier_y, fill = player_type), size = 5.25, shape = 21) +
    # Add jersey numbers
    geom_text(data = player_locations, aes(x = x_coor, y = y_coor - carrier_y, label = jerseyNumber), colour = "white", vjust = 0.36, size = 2.8) +
    # Add in arrows for possible actions
    geom_segment(data = optimal_path, aes(x = x1, y = y1 - carrier_y, xend = x2, yend = y2 - carrier_y), colour = "forestgreen", size = 1.25, arrow = arrow(length = unit(0.1, "inches"))) +
    # Re-add in punt returner
    geom_point(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = x_coor, y = y_coor - carrier_y, fill = player_type), size = 5.5, shape = 21) +
    geom_text(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = x_coor, y = y_coor - carrier_y, label = jerseyNumber), colour = "white", vjust = 0.36, size = 2.85) +
    # Add table of metrics to plot
    geom_table(data = path_table_nest, aes(x = x, y = y, label = tb), size = 2.5) +
    # Additional specs
    theme_void() +
    theme(legend.position = c(0.82,0.66),plot.background = element_rect(fill = "white"), legend.title = element_text(size = 10), legend.text = element_text(size = 10)) +
    coord_fixed(xlim = c(-2, 80), ylim = c(-40, 20)) +
    guides(fill = guide_legend(order = 1), colour = guide_colorbar(barwidth = 0.8, barheight = 8, order = 2))+
    labs(colour = "Expected Defender \nArrival Time (Seconds)", fill = "Player Type")
  
  return(path_plot)
}


# Generate a static ggplot for each frame in a play, given alpha level
generate_plots = function(game_id, play_id, alpha, updateProgress = NULL) {
  
  # Load in specified play
  play_row = play_list_df %>%
    filter(gameId == game_id & playId == play_id)
  
  szn = play_row$season[[1]]
  wk = play_row$week[[1]]
  
  load(paste0("Data/input/", szn, "/Week ", wk, "/", game_id, "_", play_id, ".Rdata"))
  
  if (is.function(updateProgress)) {
    updateProgress(detail = "Gaps")
  }
  # Compute optimal path at given alpha level
  play_data_opt = play_data %>% as.data.frame() %>%
    # Output optimal path from the A* algorithm
    mutate(optimal_path = future_map(windows, possibly(~find_optimal_path(.x, alpha = alpha, updateProgress), NULL))) %>%
    # Fill in optimal path with straight arrow ahead if there are no defenders above the carrier
    mutate(optimal_path = ifelse(defenders_above_count == 0, 
                                 future_map(carrier, ~data.frame(x1 = .x$x_coor, y1 = .x$y_coor, x2 = .x$x_coor, y2 = .x$y_coor - 5)), 
                                 optimal_path))
  if (is.function(updateProgress)) {
    updateProgress(detail = "Carrier Info")
  }
  # Extract carrier's observed path
  carrier_path = play_data_opt %>%
    select(frameId, carrier) %>%
    unnest(carrier) %>%
    ungroup()
  
  if (is.function(updateProgress)) {
    updateProgress(detail = "Path Deviation")
  }
  # Calculate Frechet distance
  play_data_frch = play_data_opt %>%
    mutate(carrier_path = map(frameId, ~carrier_path)) %>%
    # Smooth optimal path via Frechet distance
    ungroup() %>%
    mutate(required_smoothing = future_pmap(list(optimal_path, lag(optimal_path), lead(optimal_path)), possibly(smooth_path, NULL))) %>%
    unnest(required_smoothing) %>%
    mutate(optimal_path = ifelse(required_smoothing == 1, lag(optimal_path), optimal_path)) %>%
    # Determine the deviation between the actual path and the optimal path over the next 5 seconds
    mutate(path_frechet = future_pmap(list(carrier_path, optimal_path, frameId), possibly(invisible(find_path_frechet_v2), NA_real_))) %>%
    # Create table to be used in plots
    mutate(path_table = future_pmap(list(frameId, path_frechet, player_locations), create_path_table))
    
  if (is.function(updateProgress)) {
    updateProgress(detail = "Plot")
  }
  # Compute plot for each frame
  play_data_plot = play_data_frch %>%
    mutate(carrier_y = map(carrier, ~.x$y_coor[1])) %>%
    unnest(carrier_y) %>%
    mutate(field_pressure = map(windows, possibly(~.x %>% unnest(window_pts), NULL))) %>%
    mutate(frame_plot = pmap(
      list(
        carrier_y,
        kteamname,
        rteamname,
        player_locations,
        windows,
        field_pressure,
        optimal_path,
        carrier_path,
        path_table
      ),
      create_path_plot
    ))
  
  return(play_data_plot)
}



# Create gganimation of optimal path and windows
create_path_animation = function(kicking_team,
                                 returning_team,
                                 player_locations,
                                 windows,
                                 field_pressure,
                                 optimal_path,
                                 carrier_path,
                                 path_table,
                                 hash_marks,
                                 yard_lines,
                                 yard_line_text,
                                 field_outline) {
  
  # Set specs for animation
  options(gganimate.dev_args = list(width = 8, height = 6, units = 'in', res=320))
  
  xmin = 0
  xmax = 53.33
  ymin = 0
  ymax = 120
  hash.right = 38.35
  hash.left = 12
  hash.width = 3.3
  field.alpha = 0.7
  
  team_colours = choose_colours(kicking_team, returning_team)
  
  # Create animation
  animated_play = ggplot() +
    ## CREATE FIELD ##
    # Set colour and size parameters
    scale_size_manual(values = c(6, 4, 6), guide = "none") + 
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = team_colours) + 
    scale_colour_gradientn(colours = c("red", "yellow", "forestgreen"),
                           guide = guide_colorbar(barwidth = 0.8, barheight = 8)) +
    # Add hash marks
    geom_text(data = hash_marks %>% filter(x < 55/2), aes(x = x, y = y - carrier_y), label = "_", hjust = 0, vjust = -0.2, alpha = field.alpha) +
    geom_text(data = hash_marks %>% filter(x > 55/2), aes(x = x, y = y - carrier_y), label = "_", hjust = 1, vjust = -0.2, alpha = field.alpha) +
    # Add yard lines
    geom_segment(data = yard_lines, aes(x = x, y = y - carrier_y, xend = xend, yend = yend - carrier_y), alpha = field.alpha) +
    # Add yard line text
    geom_text(data = yard_line_text, aes(x = x_left, y = y - carrier_y, label = label_left), angle = 270, size = 4, alpha = field.alpha) +
    geom_text(data = yard_line_text, aes(x = x_right, y = y - carrier_y, label = label_right), angle = 90, size = 4, alpha = field.alpha) +
    # Add field exterior
    geom_segment(data = field_outline, aes(x = x, xend = xend, y = y - carrier_y, yend = yend - carrier_y), alpha = field.alpha) + 
    # Add theme
    theme_void() +
    coord_fixed(xlim = c(-2, 80), ylim = c(-40, 20)) +
    ## ADD IN PLAYERS AND WINDOWS
    # Add in static path of returner
    geom_path(data = carrier_path, aes(x = x_coor, y = y_coor - carrier_y, group = frameId), colour = "grey75", alpha = 0.6, size = 1.5) +
    # Add in horizontal line for returner
    geom_segment(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = -2, xend = 55.33, y = y_coor - carrier_y, yend = y_coor - carrier_y), linetype = "longdash", alpha = 0.7) +
    # Add windows
    geom_segment(data = windows, aes(x = x1, xend = x2, y = y1 - carrier_y, yend = y2 - carrier_y), alpha = 0.4) +
    # Add in heat points of field pressure
    geom_point(data = field_pressure, aes(x = x, y = y - carrier_y, colour = arrival_time), alpha = 0.6) +
    # Add players
    geom_point(data = player_locations, aes(x = x_coor, y = y_coor - carrier_y, fill = player_type), size = 5.25, shape = 21) +
    # Add jersey numbers
    geom_text(data = player_locations, aes(x = x_coor, y = y_coor - carrier_y, label = jerseyNumber), colour = "white", vjust = 0.36, size = 2.8) +
    # Add in arrows for possible actions
    geom_segment(data = optimal_path, aes(x = x1, y = y1 - carrier_y, xend = x2, yend = y2 - carrier_y), colour = "forestgreen", size = 1.25, arrow = arrow(length = unit(0.1, "inches"))) +
    # Re=add punt returner
    geom_point(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = x_coor, y = y_coor - carrier_y, fill = player_type), size = 5.5, shape = 21) +
    geom_text(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = x_coor, y = y_coor - carrier_y, label = jerseyNumber), colour = "white", vjust = 0.36, size = 2.85) +
    # Add table of metrics to plot
    geom_table(data = path_table, aes(x = 80, y = -30, label = path_table), size = 2.5) +
    # Additional specs
    labs(colour = "Expected Defender \nArrival Time (Seconds)", fill = "Player Type") +
    theme(legend.position = c(0.82,0.66), plot.background = element_rect(fill = "white"), legend.title = element_text(size = 10), legend.text = element_text(size = 10)) +
    transition_time(frameId) + 
    enter_fade() + 
    exit_fade()
  
  return(animated_play)
}


# Generate a gganimation for an entire play (note: alpha level is already fixed, but we need it for saving purposes)
generate_animation = function(play_data, alpha, save_as_gif = FALSE) {
  
  xmin = 0
  xmax = 53.33
  ymin = 0
  ymax = 120
  hash.right = 38.35
  hash.left = 12
  hash.width = 3.3
  field.alpha = 0.7
  
  # Extract each component of the animation as individual data frames/vectors
  kteamname = play_data$kteamname[[1]]
  rteamname = play_data$rteamname[[1]]
  
  carrier_data = play_data %>%
    select(frameId, carrier) %>%
    unnest(carrier) %>% ungroup() %>%
    select(frameId, carrier_y = y_coor)
  
  player_locations = play_data %>%
    select(frameId, event, player_locations) %>%
    unnest(player_locations) %>% ungroup() %>%
    filter(player_type != "Football") %>%
    mutate(player_type = ifelse(player_type == "Kicking Team", 
                                paste(player_type, " (", kteamname, ")", sep = ""),
                                paste(player_type, " (", rteamname, ")", sep = "")
    )) %>%
    left_join(carrier_data, by = c("frameId"))
  
  windows = play_data %>%
    select(frameId, event, windows) %>%
    unnest(windows) %>% ungroup()
  
  field_pressure = play_data %>%
    select(frameId, event, windows) %>%
    unnest(windows) %>% ungroup() %>%
    unnest(window_pts) %>% ungroup()
  
  optimal_path = play_data %>%
    select(frameId, event, optimal_path) %>%
    filter(!is.null(optimal_path)) %>%
    unnest(optimal_path) %>% ungroup() %>%
    left_join(carrier_data, by = c("frameId")) %>%
    filter(y1 >= 10)
  
  carrier_path_data = carrier_data %>%
    mutate(carrier_path = map(carrier_y, ~player_locations %>% filter(grepl("Returner", player_type)) %>% select(x_coor, y_coor))) %>%
    unnest(carrier_path) %>% ungroup()

  path_table = play_data %>%
    select(frameId, path_table) %>%
    mutate(path_table = map(
      path_table, ~.x %>%
        mutate(Value = round(Value,2)) %>%
        mutate(Value = ifelse(is.na(Value), "", Value)) %>%
        row_to_names(1)
    ))
  
  # Create field in background of animation
  hash_marks = carrier_data %>%
    mutate(hash_marks = map(carrier_y, ~expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110)) %>%
                              filter(!(floor(y %% 5) == 0)) %>%
                              filter(y < ymax, y > ymin)
    )) %>%
    unnest(hash_marks) %>% ungroup()
  
  yard_lines = carrier_data %>%
    mutate(yard_lines = map(carrier_y, ~data.frame(
      x = xmin, 
      xend =  xmax,
      y = seq(max(10, ymin), min(ymax, 110), by = 5), 
      yend = seq(max(10, ymin), min(ymax, 110), by = 5)
    ))) %>%
    unnest(yard_lines) %>% ungroup()
  
  yard_line_text = carrier_data %>%
    mutate(yard_text = map(carrier_y, ~data.frame(
      y = seq(10, 110, by = 10), 
      x_left = rep(hash.left, 11), 
      label_left = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
      x_right = rep((xmax - hash.left), 11), 
      label_right = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   ")
    ))) %>%
    unnest(yard_text) %>% ungroup()
  
  field_outline = carrier_data %>%
    mutate(exterior = map(carrier_y, ~data.frame(
      x = c(xmin, xmin, xmax, xmax), 
      y = c(ymin, ymax, ymax, ymin), 
      xend = c(xmin, xmax, xmax, xmin), 
      yend = c(ymax, ymax, ymin, ymin)
    ))) %>%
    unnest(exterior) %>% ungroup()
  
  # Create animation
  animation = create_path_animation(
    kteamname,
    rteamname,
    player_locations,
    windows,
    field_pressure,
    optimal_path,
    carrier_path_data,
    path_table,
    hash_marks,
    yard_lines,
    yard_line_text,
    field_outline
  )
  
  # Extract information to store output
  game = play_data$gameId[[1]]
  play = play_data$playId[[1]]
  max_frame = max(play_data$frameId)
  min_frame = min(play_data$frameId)
  
  return(animation)
  
}



# Create gganimation of optimal path and windows
create_path_slider = function(kicking_team,
                              returning_team,
                              player_locations,
                              windows,
                              field_pressure,
                              optimal_path,
                              carrier_path,
                              path_table,
                              hash_marks,
                              yard_lines,
                              yard_line_text,
                              field_outline,
                              height,
                              width) {
  
  # Set specs for animation
  options(gganimate.dev_args = list(width = 8, height = 6, units = 'in', res=320))
  
  xmin = 0
  xmax = 53.33
  ymin = 0
  ymax = 120
  hash.right = 38.35
  hash.left = 12
  hash.width = 3.3
  field.alpha = 0.7
  
  team_colours = choose_colours(kicking_team, returning_team)
  
  # Create animation
  animated_play = ggplot() +
    ## CREATE FIELD ##
    # Set colour and size parameters
    scale_size_manual(values = c(6, 4, 6), guide = "none") + 
    scale_shape_manual(values = c(21, 16, 21), guide = "none") +
    scale_fill_manual(values = team_colours) + 
    scale_colour_gradientn(colours = c("red", "yellow", "forestgreen"),
                           guide = guide_colorbar(barwidth = 0.8, barheight = 8)) +
    # Add hash marks
    geom_text(data = hash_marks %>% filter(x < 55/2), aes(x = x, y = y - carrier_y, frame = frameId), label = "_", hjust = 0, vjust = -0.2, alpha = field.alpha) +
    geom_text(data = hash_marks %>% filter(x > 55/2), aes(x = x, y = y - carrier_y, frame = frameId), label = "_", hjust = 1, vjust = -0.2, alpha = field.alpha) +
    # Add yard lines
    geom_segment(data = yard_lines, aes(x = x, y = y - carrier_y, xend = xend, yend = yend - carrier_y, frame = frameId), alpha = field.alpha) +
    # Add yard line text
    geom_text(data = yard_line_text, aes(x = x_left, y = y - carrier_y, label = label_left, frame = frameId), angle = 270, size = 4, alpha = field.alpha) +
    geom_text(data = yard_line_text, aes(x = x_right, y = y - carrier_y, label = label_right, frame = frameId), angle = 90, size = 4, alpha = field.alpha) +
    # Add field exterior
    geom_segment(data = field_outline, aes(x = x, xend = xend, y = y - carrier_y, yend = yend - carrier_y, frame = frameId), alpha = field.alpha) + 
    # Add theme
    theme_void() +
    coord_fixed(xlim = c(-2, 80), ylim = c(-40, 20)) +
    ## ADD IN PLAYERS AND WINDOWS
    # Add in static path of returner
    geom_path(data = carrier_path, aes(x = x_coor, y = y_coor - carrier_y, group = frameId, frame = frameId), colour = "grey75", alpha = 0.6, size = 1.5) +
    # Add in horizontal line for returner
    geom_segment(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = -2, xend = 55.33, y = y_coor - carrier_y, yend = y_coor - carrier_y, frame = frameId), linetype = "longdash", alpha = 0.7) +
    # Add windows
    geom_segment(data = windows, aes(x = x1, xend = x2, y = y1 - carrier_y, yend = y2 - carrier_y, frame = frameId), alpha = 0.4) +
    # Add in heat points of field pressure
    geom_point(data = field_pressure, aes(x = x, y = y - carrier_y, colour = arrival_time, frame = frameId), alpha = 0.6) +
    #geom_path(data = field_pressure, aes(x = x, y = y - carrier_y, colour = arrival_time, group = window_id), size = 1.4, alpha = 0.6) +
    # Add players
    geom_point(data = player_locations, aes(x = x_coor, y = y_coor - carrier_y, fill = player_type, frame = frameId), size = 5.25, shape = 21) +
    # Add jersey numbers
    geom_text(data = player_locations, aes(x = x_coor, y = y_coor - carrier_y, label = jerseyNumber, frame = frameId), colour = "white", vjust = 0.36, size = 2.8) +
    # Add in arrows for possible actions
    geom_segment(data = optimal_path, aes(x = x1, y = y1 - carrier_y, xend = x2, yend = y2 - carrier_y, frame = frameId), colour = "forestgreen", size = 1.25, arrow = arrow(length = unit(0.1, "inches"))) +
    # Re=add punt returner
    geom_point(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = x_coor, y = y_coor - carrier_y, fill = player_type, frame = frameId), size = 5.5, shape = 21) +
    geom_text(data = player_locations %>% filter(grepl("Returner", player_type)), aes(x = x_coor, y = y_coor - carrier_y, label = jerseyNumber, frame = frameId), colour = "white", vjust = 0.36, size = 2.85) +
    # Add table of metrics to plot
    labs(colour = "Expected Defender \nArrival Time (Seconds)", fill = "Player Type") +
    theme(legend.position = c(0.82,0.66), plot.background = element_rect(fill = "white"), legend.title = element_text(size = 10), legend.text = element_text(size = 10))
  
  animated_plotly = animated_play %>%
    ggplotly(tooltip = NULL, height = height, width = width) %>%
    subplot() %>%
    animation_opts(
      frame = 100, # Hz
      transition = 0, # Smooth
      easing = "linear",
      redraw = FALSE
    )

  return(animated_plotly)
}


# Generate a gganimation for an entire play (note: alpha level is already fixed, but we need it for saving purposes)
generate_slider_plot = function(play_data, alpha, height, width) {
  
  xmin = 0
  xmax = 53.33
  ymin = 0
  ymax = 120
  hash.right = 38.35
  hash.left = 12
  hash.width = 3.3
  field.alpha = 0.7
  
  # Extract each component of the animation as individual data frames/vectors
  kteamname = play_data$kteamname[[1]]
  rteamname = play_data$rteamname[[1]]
  
  carrier_data = play_data %>%
    select(frameId, carrier) %>%
    unnest(carrier) %>% ungroup() %>%
    select(frameId, carrier_y = y_coor)
  
  player_locations = play_data %>%
    select(frameId, event, player_locations) %>%
    unnest(player_locations) %>% ungroup() %>%
    filter(player_type != "Football") %>%
    mutate(player_type = ifelse(player_type == "Kicking Team", 
                                paste(player_type, " (", kteamname, ")", sep = ""),
                                paste(player_type, " (", rteamname, ")", sep = "")
    )) %>%
    left_join(carrier_data, by = c("frameId"))
  
  windows = play_data %>%
    select(frameId, event, windows) %>%
    unnest(windows) %>% ungroup()
  
  field_pressure = play_data %>%
    select(frameId, event, windows) %>%
    unnest(windows) %>% ungroup() %>%
    unnest(window_pts) %>% ungroup()
  
  optimal_path = play_data %>%
    select(frameId, event, optimal_path) %>%
    filter(!is.null(optimal_path)) %>%
    unnest(optimal_path) %>% ungroup() %>%
    left_join(carrier_data, by = c("frameId")) %>%
    filter(y1 >= 10)
  
  carrier_path_data = carrier_data %>%
    mutate(carrier_path = map(carrier_y, ~player_locations %>% filter(grepl("Returner", player_type)) %>% select(x_coor, y_coor))) %>%
    unnest(carrier_path) %>% ungroup()
  
  path_table = play_data %>%
    select(frameId, path_table) %>%
    mutate(path_table = map(
      path_table, ~.x %>%
        mutate(Value = round(Value,2)) %>%
        mutate(Value = ifelse(is.na(Value), "", Value)) %>%
        row_to_names(1)
    ))
  
  # Create field in background of animation
  hash_marks = carrier_data %>%
    mutate(hash_marks = map(carrier_y, ~expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110)) %>%
                              filter(!(floor(y %% 5) == 0)) %>%
                              filter(y < ymax, y > ymin)
    )) %>%
    unnest(hash_marks) %>% ungroup()
  
  yard_lines = carrier_data %>%
    mutate(yard_lines = map(carrier_y, ~data.frame(
      x = xmin, 
      xend =  xmax,
      y = seq(max(10, ymin), min(ymax, 110), by = 5), 
      yend = seq(max(10, ymin), min(ymax, 110), by = 5)
    ))) %>%
    unnest(yard_lines) %>% ungroup()
  
  yard_line_text = carrier_data %>%
    mutate(yard_text = map(carrier_y, ~data.frame(
      y = seq(10, 110, by = 10), 
      x_left = rep(hash.left, 11), 
      label_left = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
      x_right = rep((xmax - hash.left), 11), 
      label_right = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   ")
    ))) %>%
    unnest(yard_text) %>% ungroup()
  
  field_outline = carrier_data %>%
    mutate(exterior = map(carrier_y, ~data.frame(
      x = c(xmin, xmin, xmax, xmax), 
      y = c(ymin, ymax, ymax, ymin), 
      xend = c(xmin, xmax, xmax, xmin), 
      yend = c(ymax, ymax, ymin, ymin)
    ))) %>%
    unnest(exterior) %>% ungroup()
  
  # Create animation
  animation = create_path_slider(
    kteamname,
    rteamname,
    player_locations,
    windows,
    field_pressure,
    optimal_path,
    carrier_path_data,
    path_table,
    hash_marks,
    yard_lines,
    yard_line_text,
    field_outline,
    height,
    width
  )
  
  return(animation)
  
}




