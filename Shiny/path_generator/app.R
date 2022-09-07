
## Packages to run app locally ##
library(shiny)

# Source in required Shiny code (UI, server and global environment)
source("global.R")
source("server.R", local = TRUE)
source("ui.R", local = TRUE)

# Run Shiny app
shinyApp(ui = ui, server = server)