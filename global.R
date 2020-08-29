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
