library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)
library(bslib)
library(readr)


# Get Dataframes from R script


tournament_rankings_display <- read_csv("https://github.com/JayCampanell/global-power-rankings/raw/main/tournaments_display.csv", col_types = cols(.default = "c"))
display_rank <- read_csv("https://github.com/JayCampanell/global-power-rankings/raw/main/display_rank.csv", col_types = cols(.default = "c"))