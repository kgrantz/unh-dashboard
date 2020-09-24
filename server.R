#shinyServer(

function(input, output) {
  library(tidyverse)
  library(DT)
  library(htmltools)
  library(gridExtra)
  
  
  filename <- paste0("data/",Sys.Date(),"/processed_data.Rdata")
  
  load(filename)
  
  # TO DO: add in ability to scroll back on date
  # possible_dates <- dir("data/")
  # finds the maximum date to be "current_data"
  # displays all possible dates in dropdown menu in sidebar, defaults to current_data
  # if user selects some other date, load in Rdata from that directory
  # otherwise read in data from "current_data" directory
  
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
    p = ggplot(epi_curve_overall_week, aes(week_no, cases)) +
      geom_bar(stat="identity") +
      labs(x="Week",y="Cases", title='Total new cases diagnosed per week')+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(angle=90),
            axis.ticks.x=element_blank()) +
      theme_bw()
    p
  })
  
  
  ## STATEWIDE SITUATION ---------
  
  #Number of Cases
  output$state_case_label <- renderUI({box(
    strong("Statewide Cases"), 
    br(),
    em("Current Infections"),
    width=4,
    height=40,
    solidHeader=TRUE
  )})
  
  output$state_case <- renderUI({box(
    state_curr_cases,
    width=4,
    background=pick_color_threshold_numeric(random_numbers()[1], c(0, 5, 10, 100)),
    href="https://www.nh.gov/covid19/dashboard/active-cases.htm"
  )})
  
  #Hospitalization
  output$hospitalization_label <- renderUI({box(
    strong("Statewide Currently"),
    strong("Hospitalized"),
    width=4,
    height=40,
    solidHeader=TRUE
  )})
  
  output$hospitalization <- renderUI({box(
    state_curr_hosp,
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
    state_curr_cond,
    width=4,
    background=ifelse(restriction()=="Open","green",ifelse(restriction() == "Limited","orange","red")),
    href="https://www.covidguidance.nh.gov/"
  )})
  
  # TO DO: add in small text date at which statewide conditions were updated
  
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
                                                   choices = c(unique(tecCampusfinal$campus)), 
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
  # TO DO -- CHANGE THIS TO DIFFERENT OBJECT to pull isolation numbers
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
    br(),
    h4(strong(glue("{n_isol()}", "  (", "{n_isol_sym()}", ")"))),
    width=4, 
    height=80,
    solidHeader = TRUE)})
  
  output$n_quar <- renderUI({box(
    strong("# of Durham quarantined"), 
    br(), 
    em("Total"),
    br(),
    h4(strong(n_quar_no())),
    width=4,
    height=80,
    solidHeader = TRUE)})
  
  output$n_test <- renderUI({box(
    strong("Days from test to isolation"), 
    br(), 
    em("7-day median"),
    br(),
    h4(strong(n_test_no)),
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
  # TO DO (lower priority) -- add hover to the plot for the daily counts of each category
  
  tecCampusfinal$result <- factor(tecCampusfinal$result, levels=c("Positive", "Negative", "Invalid / Rejected / Not Performed"))
  

  
  output$testing_plot <- renderPlot({
    
    date_limits <- c(min(tecCampusfinal$date)-6, max(tecCampusfinal$date)+6)
    date_breaks = unique(tecCampusfinal$date)
    
    gtest <- ggplot() +
      geom_bar(data=subset(tecCampusfinal,campus==campus_opt()), aes(x=date, y=tests, fill=result), stat="identity") +
      scale_fill_manual(name="", values=c("#bbc1c9","#5c7596","#912931"))+
      scale_x_date(name="", breaks = date_breaks, date_labels = "%b-%d", limits = date_limits) +
      scale_y_continuous(name="") +
      theme_minimal() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle=90),
            legend.position = "top")
    
    glabel <- ggplot(data=subset(pct_pos,campus==campus_opt())) +
      geom_text(aes(x=week_no, y=5, label=pct_pos_label)) +
      geom_text(aes(x=week_no, y=10, label=n_not_subm)) +
      geom_text(aes(x=week_no, y=15, label=n_tot)) +
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
    
    grid.arrange(gtest, glabel, heights=c(7, 3))
    
  })
  
  
  ## Lab delay labels
  random_delays <- reactive({
    ceiling(runif(3, min=0, max=14))
  })
  
  output$lab_unh_label <- renderUI({box(
    h4(random_delays()[1]),
    em("UNH"),
    width=2, 
    height=80,
    solidHeader = TRUE
  )})
  
  output$lab_quest_label <- renderUI({box(
    h4(random_delays()[2]),
    em("Quest"),
    width=2, 
    height=80,
    solidHeader = TRUE
  )})
  
  output$lab_cmd_label <- renderUI({box(
    h4(random_delays()[3]),
    em("CMD"),
    width=2, 
    height=80,
    solidHeader = TRUE
  )})
  
  
}

#)
