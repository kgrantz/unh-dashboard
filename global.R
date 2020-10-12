library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(glue)
library(viridisLite)
library(tippy)
library(readr)
library(DT)
library(plotly)


## Helper functions ----

my_tippy <- function(text, help_file, placement = "right") {
  tippy(
    text = text, tooltip = includeMarkdown(help_file),
    placement = placement, animateFill = FALSE
  )
}

# can be used to build a hover help text - e.g., 
# numericInput("name", text_q("Display text", "help/help_file.md",...)
# shows an input box with "Q1. Display Text" and hover help button
text_q <- function(text, help) {
  tagList(
    text,
    icon("question-circle") %>% my_tippy(help)
  )
}

# if a value is less than 5, display "1 - 5" - requested by UNH for confidentiality
print_under5 <- function(x){
  if(is.na(x)){return("NA")}else{
    if(x>5){return(as.character(x))}
    if(x>0 & x<5){return("1 - 5")}
    if(x==0){return("0")}
  }
}

# pick the color based on numeric level of cutoff
pick_color_threshold_numeric <- function(value, 
                                         thresholds, 
                                         colors=c("green", "yellow", "orange", "red")){
  return(colors[max(which(value > thresholds))])
}
  

##getting list of dates available for UI
data_dates <- as.Date(list.dirs(path = "./data", full.names = FALSE, recursive = FALSE))

##all dates from 2020-09-23 to present
all_dates <- seq(from=as.Date("2020-09-23"),Sys.Date(),"days")

# remove dates for which pre - processing script doesn't run
remove_dates <- as.Date(all_dates[is.na(match(all_dates,data_dates))])

int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}
  
