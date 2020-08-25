function(input, output, session) {
  Sys.sleep(2)
  waiter_hide()
  
  ## Home page -----------------------------------------------------------------
  # output$date_updated <- renderText({glue("Last updated: ", "{Sys.Date()}")}) # TO DO: update this with data load
  
  # TO DO: replace this with function that builds epi curve for all data
  output$epi_curve_total <- renderPlot({
    ggplot(data=data.frame(x=1:10, y=1:10), aes(x, y)) + geom_point()
  })
  
  # TO DO: read in/format statewide values to create boxes with appropriate colors
  output$state_cases <- renderUI({
    box(
      p("# positive cases"), 
      p("14-day"),
      background = "blue",
      width=4
    )
  })
  
  ## UNH Campus Situation -------------------------------------------------------
  
  # TO DO: replace this with function that builds epi curve for selected campus
  # using input$campus
  # Function should also take argument input$epi_curve_type to determine whether
  # the epi curve bars are split by role (student/employee) or on/off campus
  output$epi_curve_detail <- renderPlot({hist(rnorm(100), main="Epi Curve - Detailed", col="cornflowerblue")})
  
  # TO DO: calculate total under isolation for given input$campus
  n_isol <- 7
  
  # TO DO: calculate total under isolation + symptomatic for given input$campus
  n_isol_sym <- 5
  
  # creating pretty string with total isolated + symptomatic
  # TO DO: add in tests to make sure these numbers are reasonable (e.g., sym <= total)
  # TO DO: make this a function to renderUI the entire box, not just value to be passed to UI
  output$n_isol_label <- renderText({glue("{n_isol}", " (", "{n_isol_sym}", ")")})
  
  ## Lab Testing --------------------------------------------------------
  
}
