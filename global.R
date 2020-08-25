library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(glue)
library(viridisLite)
library(tippy)

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
