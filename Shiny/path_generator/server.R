
server = function(input, output, session) {
  
  ## SETTING UP REACTIVE UI DROPDOWN MENUS/SLIDERS ##
  
  # Filter for possible returners in season
  get_returners = eventReactive(input$season,{
    req(input$season)
    returners = play_list_df %>%
      filter(season == input$season) %>%
      mutate(returner_team = paste0(returner, " (", rteamname, ")")) %>%
      select(returner_team) %>%
      unique() %>%
      unlist() %>%
      unname() %>%
      sort()
    return(returners)
  })
  
  # Select returner
  observeEvent(input$season,{
    updateSelectizeInput(session, input = "returner", choices = c(" ",get_returners()))#, selected = "Mecole Hardman (KC)")
  })
  
  observeEvent(input$returner,{
    output$headshot <- renderImage({
      if(input$returner!=' '){
        filename <- normalizePath(file.path(paste("headshots/",tolower(gsub("__","_",gsub("[[:punct:]]", "_", gsub(" ","_",gsub("\\s*\\([^\\)]+\\)","",as.character(input$returner)))))),".jpg",sep='')))
        list(src = filename, alt="Alternate text", width="60%")
      }else{
        filename <- normalizePath(file.path("headshots/returner.jpg"))
        list(src = filename, alt="Alternate text", width="60%")
      }
    }, deleteFile = FALSE)
  })
  
  # Filter for punt returns by selected returner
  get_plays = eventReactive(input$returner,{
    req(input$returner)
    if(input$returner!=" "){
      chosen_returner = gsub("\\s*\\([^\\)]+\\)", "", input$returner)
      plays = play_list_df %>%
        filter(season == input$season & returner == chosen_returner) %>%
        mutate(play_name = paste("vs", kteamname, "-", "Week", week, "-", "Quarter", quarter, "-", clock)) %>%
        select(play_name) %>%
        unique() %>%
        unlist() %>%
        unname()
      
      return(plays)
    }
    
  })
  
  # Select punt return
  observeEvent(input$returner,{
    updateSelectizeInput(session, input = "play", choices = c(" ",get_plays()))
  })
  
  # Print play description
  observeEvent(input$play, {
    output$description = renderText(
      play_list_df %>%
        mutate(play_name = paste("vs", kteamname, "-", "Week", week, "-", "Quarter", quarter, "-", clock)) %>%
        filter(play_name == input$play) %>%
        select(playDescription) %>%
        unlist() %>%
        unname()
    )
  })
  
  # Once a plot has been selected...
  observeEvent(
    input$generate_plot,
    {
      req(input$generate_plot) #play
      
      progress <- shiny::Progress$new()
      progress$set(message = "Loading: ", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      
      play_row = play_list_df %>%
        mutate(play_name = paste("vs", kteamname, "-", "Week", week, "-", "Quarter", quarter, "-", clock)) %>%
        filter(play_name == input$play)
      
      game_id = play_row$gameId
      play_id = play_row$playId
      
      full_play = generate_plots(game_id, play_id, alpha = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha)), updateProgress)
      
      # Plot optimal path at frame
      
      updateSliderInput(session, "seconds", max =  max(full_play$seconds))
      
      output$path_plot = renderPlot(
        full_play$frame_plot[[10*(input$seconds + 0.1)]],
        width = 600,
        height = 600
      )
      
      # Get game and play ID of punt return
      get_ids = eventReactive(input$play,{
        req(input$play)
        play_row = play_list_df %>%
          mutate(play_name = paste("vs", kteamname, "-", "Week", week, "-", "Quarter", quarter, "-", clock)) %>%
          filter(play_name == input$play)
        game_id = play_row$gameId
        play_id = play_row$playId
        
        return(c(game_id, play_id))
      })
      
      output$save_png = downloadHandler(
        filename = function(){
          ids = get_ids()
          alpha_num = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha))
          return(paste(ids[1], "_", ids[2], "_frame", full_play$frameId[[10*(input$seconds + 0.1)]], "_alpha", alpha_num*100, "_plot.png",sep=''))
        },
        content = function(file){
          ids = get_ids()
        
          alpha_num = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha))
          
          ggsave(file, full_play$frame_plot[[10*(input$seconds + 0.1)]], device = "png", width = 7, height = 8)
          
        },
        contentType = "image/png"
      )
          

      output$save_gif = downloadHandler(
        
        filename = function(){
          ids = get_ids()
          alpha_num = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha))
          return(paste(ids[1], "_", ids[2], "_alpha", alpha_num*100, "_plot.gif",sep=''))
        },
        content = function(file){
          withProgress(message = 'Saving gif', value = 0, {
          alpha_num = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha))
          incProgress(1/3, detail = "Generating")
          animation = generate_animation(full_play, alpha = alpha_num, save_as_gif = TRUE)
          
          max_frame = max(full_play$frameId)
          min_frame = min(full_play$frameId)
          incProgress(1/3, detail = "Rendering")
          animate(animation, fps = 10, duration = (max_frame - min_frame) / 10, renderer = gifski_renderer())
          incProgress(1/3, detail = "Saving")
          anim_save(file)
          })
        },
        contentType = NA
      )
      
      output$save_rdata = downloadHandler(
        filename = function(){
          ids = get_ids()
          alpha_num = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha))
          return(paste(ids[1], "_", ids[2], "_alpha", alpha_num*100, "_plot.Rdata",sep=''))
        },
        content = function(file){
          
          alpha_num = case_when(input$alpha == "Risky" ~ 1, input$alpha == "Safe" ~ 0, TRUE ~ as.numeric(input$alpha))
          animation = generate_animation(full_play, alpha = alpha_num, save_as_gif = TRUE)
          
          max_frame = max(full_play$frameId)
          min_frame = min(full_play$frameId)
          
          save(animation, file = file)
        },
        contentType = NA
      )
      
    }
  )
  
  
}


