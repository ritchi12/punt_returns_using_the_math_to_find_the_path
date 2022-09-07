
# Load packages
library(shinyWidgets)
library(tidyverse, warn.conflicts=FALSE)
library(tripack)
library(transformr)
library(gganimate)
library(gifski)
library(sp)
library(StereoMorph)
library(janitor)
library(splancs)
library(reshape2)
library(sf)
library(kmlShape)
library(furrr)
library(tictoc)
library(ggpmisc)
library(tm)
library(plotly)
library(shinythemes)


options(dplyr.summarise.inform = FALSE)

# Load necessary data
play_list_df = read_csv("Data/input/full_play_list.csv")
colours = read_csv("Data/raw/nfl_cols.csv", col_names = FALSE)

# Source in custom functions that will be used in Shiny app
source("shiny_functions.R")