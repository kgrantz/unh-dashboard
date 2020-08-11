function(input, output, session) {
  Sys.sleep(2)
  waiter_hide()
  
  ## Disconnect --------------------------------------------------------------
  
  sever(
    html = tagList(
      h2("it looks like you've disconnected."),
      br(),
      spin_loaders(id = 11, color = "#f1c400"),
      br(), br(), br(),
      reload_button("Click here to reload",
                    class = "success"
      ),
      h3("Johns Hopkins Bloomberg School of Public Health")
    ),
    bg_color = "#265CA4", opacity = 0.9
  )
  
  ## Home page -----------------------------------------------------------------
  
  ## UNH Campus Situation -------------------------------------------------------
  
  ## Lab Testing --------------------------------------------------------
  
  ## NH State Page ---------------------------------------------------------------
  output$prepared_by_out <- renderUI(glue("Testing Output"))
}
