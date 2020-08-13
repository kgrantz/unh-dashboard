library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(glue)
library(viridisLite)

# other commonly used functions can go here


load_data <- function(){
  dat <- read.csv("data/recent_data.csv")
  return(dat)
}