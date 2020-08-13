function(input, output, session) {
  Sys.sleep(2)
  waiter_hide()
  

  ## Home page -----------------------------------------------------------------
  output$epi_curve <- renderPlot(hist(rnorm(100)))
  
  ## UNH Campus Situation -------------------------------------------------------
  
  ## Lab Testing --------------------------------------------------------
  
  ## NH State Page ---------------------------------------------------------------
  output$prepared_by_out <- renderUI(glue("Testing Output"))
}
