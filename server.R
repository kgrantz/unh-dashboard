shinyServer(
  
  function(input, output) {
  #Sys.sleep(2)
  #waiter_hide()
  
  ## Home page -----------------------------------------------------------------
  # output$date_updated <- renderText({glue("Last updated: ", "{Sys.Date()}")}) # TO DO: update this with data load
  
  # TO DO: replace this with function that builds epi curve for all data
  
    #Created a reactive dataset. Sometimes render functions work better with dynamic output
    
    data_random <- reactive({ 
    data.frame(x=sample(1:10),y=sample(1:10))
    
  })
  
  
  #plotting using reactive dataset
  output$epi_curve_total <- renderPlot({
    p = ggplot(data=data_random(), aes(x, y)) + geom_point()
    p
    })
  
  
  #randomly generates 3 numbers for state wide values
  random_numbers <- reactive({
    runif(3, min=0, max=100)
    })
  
  
  
  
# An idea to generate state wide boxes color coded on value using the randomly generated numbers above
#Number of Cases
output$state_case <- renderValueBox({valueBox(p(ceiling(random_numbers()[1])),"positive cases",
                                              width=4,
                                              color=ifelse(random_numbers()[1] > 50,"blue","yellow"),
                                              href="https://www.nh.gov/covid19/dashboard/active-cases.htm"
)})

#Hospitalization
output$hospitalization <- renderValueBox({valueBox(p(ceiling(random_numbers()[2])),"hospitalizations",
                                                   width=4,
                                                   color=ifelse(random_numbers()[2] > 50,"blue","yellow"),
                                                   href="https://www.nh.gov/covid19/dashboard/active-cases.htm"
)})


#Current Restrictions  
output$current_restrictions <- renderValueBox({valueBox(p(ceiling(random_numbers()[2])),"Current Restrictions",
                                                        width=4,
                                                        color=ifelse(random_numbers()[2] > 50,"blue","yellow"),
                                                        href="https://www.covidguidance.nh.gov/"
)})
  
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

)
