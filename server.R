#shinyServer(

function(input, output) {
  library(tidyverse)
  library(DT)
  library(htmltools)
  library(gridExtra)
  
  #Sys.sleep(2)
  #waiter_hide()
  
  ## Home page -----------------------------------------------------------------
  # output$date_updated <- renderText({glue("Last updated: ", "{Sys.Date()}")}) # TO DO: update this with data load
  
  ## EPI CURVE ---------
  data_random <- reactive({ 
    data.frame(x=sample(1:10),y=sample(1:10))
    
  })
  
  # TO DO: replace this with function that builds epi curve for all data
  output$epi_curve_total <- renderPlot({
    p = ggplot(data=data_random(), aes(x, y)) + geom_point()
    p
  })
  
  
  ## STATEWIDE SITUATION ---------
  #randomly generates 3 numbers for state wide values
  random_numbers <- reactive({
    runif(2, min=0, max=100)
  })
  
  restriction <- reactive({
    sample(c("Limited","Widespread","Open"),1)  
  })
  
  #Number of Cases
  output$state_case_label <- renderUI({box(
    strong("Statewide Cases"), 
    br(),
    em("14-day total"),
    width=4,
    height=40,
    solidHeader=TRUE
  )})
  
  output$state_case <- renderUI({box(
    ceiling(random_numbers()[1]),
    width=4,
    background=pick_color_threshold_numeric(random_numbers()[1], c(0, 5, 10, 100)),
    href="https://www.nh.gov/covid19/dashboard/active-cases.htm"
  )})
  
  #Hospitalization
  output$hospitalization_label <- renderUI({box(
    strong("Currently"),
    strong("Hospitalized"),
    width=4,
    height=40,
    solidHeader=TRUE
  )})
  
  output$hospitalization <- renderUI({box(
    ceiling(random_numbers()[2]),
    width=4,
    background=pick_color_threshold_numeric(random_numbers()[2], c(0, 5, 10, 100)),
    href="https://www.nh.gov/covid19/dashboard/active-cases.htm"
  )})
  
  
  #Current Restrictions
  output$current_restrictions_label <- renderUI({box(
    strong("Current"),
    strong("Restrictions"),
    width=4,
    height=40,
    solidHeader=TRUE
  )})
  
  output$current_restrictions <- renderUI({box(
    restriction(),
    width=4,
    background=ifelse(restriction()=="Open","green",ifelse(restriction() == "Limited","orange","red")),
    href="https://www.covidguidance.nh.gov/"
  )})
  
  
  ## CAMPUS SITUATION ---------
  
  ## active cases
  random_active_cases <- reactive({
    sample(0:200, 3)
  })
  
  output$active_cases_durham <- renderUI({box(
    random_active_cases()[1],
    width=3, 
    height=80,
    background=pick_color_threshold_numeric(random_active_cases()[1], c(0, 10, 50, 200)),
    solidHeader = TRUE
  )})
  
  output$active_cases_manch <- renderUI({box(
    random_active_cases()[2],
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_active_cases()[2], c(0, 10, 50, 200)),
    solidHeader=TRUE
  )})
  
  output$active_cases_concord <- renderUI({box(
    random_active_cases()[3],
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_active_cases()[3], c(0, 10, 50, 200)),
    solidHeader=TRUE
  )})
  
  ## case rates
  random_case_rates <- reactive({
    sample(0:200, 3)
  })
  
  output$case_rates_durham <- renderUI({box(
    glue("{random_case_rates()[1]} per 1000"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_case_rates()[1], c(0, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  output$case_rates_manch <- renderUI({box(
    glue("{random_case_rates()[2]} per 1000"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_case_rates()[2], c(0, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  output$case_rates_concord <- renderUI({box(
    glue("{random_case_rates()[3]} per 1000"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_case_rates()[3], c(0, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  ## pct isol
  random_pct_isol <- reactive({
    sample(0:100, 3)
  })
  
  output$pct_isol_durham <- renderUI({box(
    glue("{random_pct_isol()[1]}%"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_pct_isol()[1], c(0, 10, 50, 90)),
    solidHeader=TRUE
  )})
  
  output$pct_isol_manch <- renderUI({box(
    glue("{random_pct_isol()[2]}%"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_pct_isol()[2], c(0, 10, 50, 90))
  )})
  
  output$pct_isol_concord <- renderUI({box(
    glue("{random_pct_isol()[3]}%"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_pct_isol()[3], c(0, 10, 50, 90))
  )})
  
  ## pct quar
  random_pct_quar <- reactive({
    sample(0:100, 3)
  })
  
  output$pct_quar_durham <- renderUI({box(
    glue("{random_pct_quar()[1]}%"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_pct_quar()[1], c(0, 25, 50, 90))
  )})
  
  output$pct_quar_manch <- renderUI({box(
    glue("{random_pct_quar()[2]}%"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_pct_quar()[2], c(0, 25, 50, 90))
  )})
  
  output$pct_quar_concord <- renderUI({box(
    glue("{random_pct_quar()[3]}%"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(random_pct_quar()[3], c(0, 25, 50, 90))
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
      scale_fill_manual(name="", values=c('darkblue','cornflowerblue'))+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(angle=90),
            axis.ticks.x=element_blank()) +
      theme_bw()
    
  })
  
  
  #plot for student/faculty split
  output$personnel_plot <- renderPlot({
    
    ggplot(subset(campus_data, college==campus_opt() & id=="person"), aes(week_no, cases,fill=level)) +
      geom_bar(stat="identity") +
      labs(x="Week",y="Cases", title='Total new cases diagnosed per week among students and faculty')+
      scale_fill_manual(name="", values=c('darkblue','cornflowerblue'))+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(angle=90),
            axis.ticks.x=element_blank()) +
      theme_bw()
    
  })
  
  
  # TO DO: make sure calculation of number isolated matches final data format
  n_isol <- reactive({sum(subset(campus_data,college==campus_opt()) $isolation)})
  
  # TO DO: make sure calculation of number isolated + symptomatic matches final data format
  n_isol_sym <- reactive({sum(subset(campus_data,college==campus_opt()) $symptomatic)})
  
  # TO DO: make sure calculation of number isolated + symptomatic matches final data format
  n_quar_no <-  reactive({sum(subset(campus_data,college==campus_opt()) $quarantine)})
  
  ## don't know what this number is to put in dummy data. TO DO: Find out what this number is
  n_test_no <- 1
  
 # output$n_isol_label <- renderUI({box(
  #  strong("# active cases in isolation"),
   # br(), 
    #em("Total (symptomatic)"),
    #width=4, 
    #height=80)})
  
  #output$n_isol_value <- renderUI({box(
   # h4(glue("{n_isol()}", "  (", "{n_isol_sym()}", ")")),
    #width=4, 
    #height=60)})
  
  #output$n_quar_label <- renderUI({box(
   # strong("# of Durham quarantined"), 
    #br(), 
    #em("Total"),
    #width=4,
    #height=80
  #)})
  
  #output$n_quar_value <- renderUI({box(
   # h4(n_quar_no()),
    #width=4,
    #height=60)})
  
  #output$n_test_label <- renderUI({box(
   # strong("Days from test to isolation"), 
    #em("7-day median"),
    #width=4,
    #height=80)})
  
  #output$n_test_value <- renderUI({box(
   # h4(n_test_no),
  #  width=4,
   # height=60)})
  
  
  #made formatted boxes for the 3 box tabs in campus tab. Please feel free to change color, size etc. according to
  #other preferences
  output$n_isol_label <- renderUI({box(
    strong("# active cases in isolation"),
    br(), 
    em("Total  (symptomatic)"),
    h5(strong(glue("{n_isol()}", "  (", "{n_isol_sym()}", ")"))),
    width=4, 
    height=80,
    solidHeader = TRUE)})
  
  output$n_quar <- renderUI({box(
    strong("# of Durham quarantined"), 
    br(), 
    em("Total"),
    h5(strong(n_quar_no())),
    width=4,
    height=80,
    solidHeader = TRUE)})
  
  output$n_test <- renderUI({box(
    strong("Days from test to isolation"), 
    br(), 
    em("7-day median"),
    h5(strong(n_test_no)),
    width=4,
    height=80,
    solidHeader = TRUE)})
  
  
  
  # TO DO: create function to process this data from whatever final format lab data takes
  # will depend on campus_opt()
  # (from Kyra) I'm not sure this needs to be datatable vs static form like kableExtra? formatting still needs work
  Dorm <- c("Dorm A","Dorm B","Off-campus")
  Positive_tests <- c(2,3,1)
  perc_positive <- c(0.02,0.07,0.01)
  n_quart <- c(31,45,14)
  
  Dorm_tab <- cbind(Dorm,Positive_tests,perc_positive,n_quart)
  
  #table out to UI
  dorm_table = htmltools::withTags(table(
    style = "font-size: 100%; width: 95%; height:70%;  background-color:#FFFFFF;margin-top = -1em",
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, "Dorm"),
        th(colspan = 1, "Positive tests"),
        th(colspan = 1, "% Positivity"),
        th(colspan = 1, "# Quarantined")
      ),
      tr(
        th("(14-day total)"),
        th("(14-day average)"),
        th("(% of all beds)")
      )
      
    )
  ))
  
  
  output$mytable = renderDataTable({
    DT::datatable(
      Dorm_tab,
      options = list(dom="t"),
      container = dorm_table
    )
  })
  
  
  ## Lab Testing --------------------------------------------------------
  
  ## Lab testing plot/numbers
  test_data <- data.frame(date = as.Date(rep(c("2020-06-23", "2020-06-29", "2020-07-06", 
                                               "2020-07-13", "2020-07-20", "2020-07-27", 
                                               "2020-08-03"), each=3)),
                          test_type = rep(c("Pending", "Negative", "Positive"), times=7),
                          n_test = sample(1:100, 21, replace=TRUE))
  test_data$test_type <- factor(test_data$test_type, levels=c("Pending", "Negative", "Positive"))
  
  pct_pos <- test_data %>% 
             group_by(date) %>% 
             summarise(n_pos = n_test[which(test_type=="Positive")],
                       n_neg = n_test[which(test_type=="Negative")],
                       n_tot = sum(n_test),
                       n_not_subm = round(n_tot*0.1),
                       pct_pos = n_pos/ (n_pos + n_neg),
                       pct_pos_label = glue("{round(pct_pos*100,1)}%"))
  
  output$testing_plot <- renderPlot({
    
    date_limits <- c(min(test_data$date)-6, max(test_data$date)+6)
    date_breaks = unique(test_data$date)
    
    gtest <- ggplot() +
              geom_bar(data=test_data, aes(x=date, y=n_test, fill=test_type), stat="identity") +
              scale_fill_manual(name="", values=c("#bbc1c9","#5c7596","#912931"))+
              scale_x_date(name="", breaks = date_breaks, date_labels = "%b-%d", limits = date_limits) +
              scale_y_continuous(name="") +
              theme_minimal() +
              theme(axis.ticks.x=element_blank(),
                    legend.position = "top")
    
    glabel <- ggplot(pct_pos) +
               geom_text(aes(x=date, y=5, label=pct_pos_label)) +
               geom_text(aes(x=date, y=10, label=n_not_subm)) +
               geom_text(aes(x=date, y=15, label=n_tot)) +
               scale_x_date(name="", limits = date_limits) +
               scale_y_continuous(name="", breaks=c(5, 10, 15), labels=c("% Positive", "# Not Submitted", "# Submitted")) +
               theme_minimal() +
               theme(
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size=11),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.title = element_blank()
               )
    
    glabel <- ggplot_gtable(ggplot_build(glabel))
    gtest <- ggplot_gtable(ggplot_build(gtest))
    gtest$widths <-glabel$widths 
    
    grid.arrange(gtest, glabel, heights=c(10, 3))
    
  })
  
  
  ## Lab delay labels
  random_delays <- reactive({
    sample(0:14, 3, replace=TRUE)
  })
  
  output$lab_unh_label <- renderUI({box(
    h5(random_delays[1]),
    em("UNH"),
    width=3, 
    height=80,
    solidHeader = TRUE)})
  
  output$lab_quest_label <- renderUI({box(
    h5(random_delays[2]),
    em("Quest"),
    width=3, 
    height=80,
    solidHeader = TRUE)})
  
  output$lab_cmd_label <- renderUI({box(
    h5(random_delays[3]),
    em("CMD"),
    width=3, 
    height=80,
    solidHeader = TRUE)})
  
  
}

#)
