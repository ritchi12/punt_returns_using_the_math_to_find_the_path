

ui = fluidPage(theme = shinytheme("superhero"),
  
  titlePanel("Optimal Path Generator"),
  
  ## Link relevant websites ##
  HTML('
      <nav class="navbar navbar-default navbar-static-top">
        <div class="container">
          <div>
            <ul class="nav navbar-nav col-xs-12">
              <li class="col-xs-8 col-md-9 github-link">
                <a href="https://www.kaggle.com/code/robynritchie/punt-returns-using-the-math-to-find-the-path/notebook?scriptVersionId=84537262" target="_blank">
                  <span class="hidden-xs">BDB Submission, Punt Returns: Using the Math to find the Path on </span>Kaggle
                </a>
              </li>
              <li class="col-xs-4 col-md-3 github-link">
                <a href="https://github.com/ritchi12/punt_returns_using_the_math_to_find_the_path" target="_blank">
                  <span class="hidden-xs">Code on </span>GitHub
                </a>
              </li>
            </ul>
          </div>
        </div>
      </nav>
    '),
  
  sidebarLayout(
    
    ## SELECTING A PUNT RETURN ##
    sidebarPanel(
      titlePanel("Select Punt Return"),
      fluidRow(
        column(
          width = 12,
          selectizeInput(
            inputId = "season",
            label = "Select Season:",
            choices = c(" ",2018,2019,2020),
            multiple=FALSE),
          selectizeInput(
            inputId = "returner",
            label = "Select Returner:",
            choices = NULL,
            multiple=FALSE),
          selectizeInput(
            inputId = "play",
            label = "Select Play:",
            choices = NULL,
            multiple=FALSE),
           sliderTextInput(
             "alpha",
             "Risk Parameter",
             choices = c("Safe", seq(0.1,0.9,0.1), "Risky"),
             selected = 0.5,
             grid = TRUE
           )
        ),
        actionButton("generate_plot", "Generate")
      )
    ),
    
    ## PLOTTING THE PUNT RETURN ##
    mainPanel(
      fixedRow(#style = "height: 75px;",
        column(8, plotOutput(outputId = "path_plot", width = "100%")),
        column(4, 
               fixedRow(#uiOutput("seconds"),
                 sliderInput("seconds", "Seconds Since Catch", min = 0, max = 0, value = 0, step = 0.1, animate =
                               animationOptions(interval = 1000, loop = FALSE)),
                        column(12, br()),
                        column(10, textOutput(outputId = "description")),
                        imageOutput("headshot"),
                        column(10,p("Important: Pause animation prior to saving.")),
                        column(10,p("Note: Gif could take several minutes to save.")),
                        column(12, downloadButton("save_png", "Save png of moment")),
                        column(12, br()),
                        column(12, downloadButton("save_gif", "Save gif of return")),
                        column(12, br()),
                        column(12, downloadButton("save_rdata", "Save Rdata of return"))
                        )
               
               )
      )
    )
  )
)
