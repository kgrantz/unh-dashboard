#shinyServer(
  
  function(input, output) {
    library(tidyverse)
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
      runif(2, min=0, max=100)
    })
    
    restriction <- reactive({
    sample(c("limited opening","closed","open"),1)  
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
    output$current_restrictions <- renderValueBox({valueBox(p(restriction()),"Current situation",
                                                            width=4,
                                                            color=ifelse(restriction()=="limited opening","lime",ifelse(restriction() == "open","orange","fuchsia")),
                                                            href="https://www.covidguidance.nh.gov/"
    )})
    
    ## UNH Campus Situation -------------------------------------------------------
    
    # TO DO: replace this with function that builds epi curve for selected campus
    # using input$campus
    # Function should also take argument input$epi_curve_type to determine whether
    # the epi curve bars are split by role (student/employee) or on/off campus
    
    
    
    #created a dummy data for campus tab in .csv. I am reading the file from my github repo. File is also at https://github.com/kgrantz/unh-dashboard/tree/master/fakedata.
    campus_data<-read.csv(url("https://raw.githubusercontent.com/sowmyavs1992/data_science_1/master/dummy_data_campus_curve.csv"))
    

    #dynamic sidebar menu conditional on selecting campus tab. Wrote a render function to reduce UI clutter.
    output$campus_dropdown <- renderMenu(selectInput("Campus", label="Select:", 
                                                     choices = c("Durham","Concord"), 
                                                     selected = "Durham")
    )
    
    #getting campus value from UI
    campus_opt <- reactive(input$Campus)
    
    
    #dynamic header for campus tab
    output$campus_text <- renderText({
                              paste("Confirmed COVID-19 cases in",input$Campus,sep=" ")
                              })
    
    #plot for On/Off Campus split
    output$location_plot <- renderPlot({
      
      ggplot(subset(campus_data, college==campus_opt() & id=="campus" ), aes(week_no, cases,fill=level)) +
        geom_bar(stat="identity") +
        labs(x="Week",y="Cases", title='Total new cases diagnosed per week on/off campus')+
        scale_fill_manual(values=c('darkblue','cornflowerblue'))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle=90),
              axis.ticks.x=element_blank())
      
      
      
      
    })
    
    
    #plot for student/faculty split
    output$personnel_plot <- renderPlot({
      
      ggplot(subset(campus_data, college==campus_opt() & id=="person"), aes(week_no, cases,fill=level)) +
        geom_bar(stat="identity") +
        labs(x="Week",y="Cases", title='Total new cases diagnosed per week among students and faculty')+
        scale_fill_manual(values=c('darkblue','cornflowerblue'))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_text(angle=90),
              axis.ticks.x=element_blank())
      
      
      
      
    })
    
    
    
    
    #output$epi_curve_detail <- renderPlot({hist(rnorm(100), main="Epi Curve - Detailed", col="cornflowerblue")})
    
    # TO DO: calculate total under isolation for given input$campus. Getting these numbers from the column in dummy data
    #Not sure if the write way to calculate
    n_isol <- reactive({sum(subset(campus_data,college==campus_opt()) $isolation)})
    
    # TO DO: calculate total under isolation + symptomatic for given input$campus
    n_isol_sym <- reactive({sum(subset(campus_data,college==campus_opt()) $symptomatic)})
    
    
    # #quarantine
    n_quar_no <-  reactive({sum(subset(campus_data,college==campus_opt()) $quarantine)})
    
    
    ## don't know what this number is to put in dummy data. TO DO: Find out what this number is
    n_test_no <- 1
    
    # creating pretty string with total isolated + symptomatic
    # TO DO: add in tests to make sure these numbers are reasonable (e.g., sym <= total)
    
    #made formatted boxes for the 3 box tabs in campus tab. Please feel free to change color, size etc. according to
    #other preferences
    output$n_isol_label <- renderUI({box(
      strong("# active cases in isolation"),br(), em("Total  (symptomatic)"),
      h4(glue("{n_isol()}", "  (", "{n_isol_sym()}", ")")),width=4, height=100,solidHeader = TRUE,background="black")})
    
    output$n_quar <- renderUI({box(
      strong("# of Durham quarantined"), br(), em("Total"),
      h4(n_quar_no()),width=4,height=100,solidHeader = TRUE)})
    
    output$n_test <- renderUI({box(
      strong("Days from test to isolation"), br(), em("7-day median"),
      h4(n_test_no),width=4,height=100,solidHeader = TRUE,background="orange")})
    
    #hard coded this table. #TO find out what these numbers mean, create a dummy table for them
    #difficult to create a subtitle in table header in shiny. TO DO: see if there is a way
    Dorm <- c("Dorm A","Dorm B","Off-campus")
    Positive_tests <- c(2,3,1)
    perc_positive <- c(0.02,0.07,0.01)
    n_quart <- c(31,45,14)
    
    Dorm_tab <- cbind(Dorm,Positive_tests,perc_positive,n_quart)
    
    
    colnames(Dorm_tab) <- c("Dorm","Positive tests","% Positivity", "# Quarantined")
    
    
    
    
    
    #table out to UI
    output$mytable = renderDataTable({
      DT::datatable(Dorm_tab,options = list(dom="t"))
    })
    
    
    
    
    
    
    
    ## Lab Testing --------------------------------------------------------
    
  }
  
#)
