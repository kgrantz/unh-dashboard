#shinyServer(

function(input, output) {
  library(tidyverse)
  library(DT)
  library(htmltools)
  library(gridExtra)
  data_dates <- as.Date(list.dirs(path = "./data", full.names = FALSE, recursive = FALSE))
  
  all_dates <- seq(from=as.Date("2020-09-23"),Sys.Date(),"days")
  
  remove_dates <- as.Date(all_dates[is.na(match(all_dates,data_dates))])
  
  observeEvent(input$InputDate,{ 

  filter_date=as.character(input$InputDate)
  
  filename <- paste0("data/",filter_date,"/processed_data.Rdata")
  
  load(filename)
  
  # TO DO: in UI, restrict available date choices to those with data available
  # OR: just roll back to the date with available data <= selected date; should add a label to sidebar saying 
  # date of data is being used, since it might not match selection
  
  #Sys.sleep(2)
  #waiter_hide()
  
  ## Home page -----------------------------------------------------------------
  output$date_updated <- renderText({glue("Last updated: ", "{updated.date}")})
  
  ## EPI CURVE ---------
  data_random <- reactive({ 
    data.frame(x=sample(1:10),y=sample(1:10))
    
  })
  
  output$epi_curve_total <- renderPlot({
    p = ggplot(epi_curve_overall_week, aes(week_start_date, cases)) +
          geom_bar(stat="identity", fill="darkblue") +
          labs(x="Week",y="Cases", title='Total new cases diagnosed per week') +
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
    background=pick_color_threshold_numeric(state_curr_cases, c(0, 100, 1200, 1800)),
    href="https://www.nh.gov/covid19/dashboard/active-cases.htm"
  )})
  
  #Hospitalization
  output$hospitalization_label <- renderUI({box(
    strong("Statewide"),
    strong("Hospitalized"),
    width=4,
    height=40,
    solidHeader=TRUE
  )})
  
  output$hospitalization <- renderUI({box(
    state_curr_hosp,
    width=4,
    background=pick_color_threshold_numeric(state_curr_hosp, c(0, 25, 50, 100)),
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
    background=ifelse(state_curr_cond=="None",
                      "green",
                      ifelse(state_curr_cond == "Open",
                             "yellow",
                             ifelse(state_curr_cond=="Limited Open", "orange", "red"))),
    href="https://www.covidguidance.nh.gov/"
  )})
  
  output$state_date_updated <- renderText({glue("State conditions last updated: ", "{state.updated.date}")})
  
  ## CAMPUS SITUATION ---------
  
  ## active cases
  random_active_cases <- reactive({
    sample(0:200, 3)
  })
  
  output$active_cases_durham <- renderUI({box(
    threshdf[1,2],
    width=3, 
    height=80,
    background=pick_color_threshold_numeric(threshdf[1,2], c(-1, 10, 50, 200)),
    solidHeader = TRUE
  )})
  
  output$active_cases_manch <- renderUI({box(
    threshdf[2,2],
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[2,2], c(-1, 10, 50, 200)),
    solidHeader=TRUE
  )})
  
  output$active_cases_concord <- renderUI({box(
    threshdf[3,2],
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[3,2], c(-1, 10, 50, 200)),
    solidHeader=TRUE
  )})
  
  ## case rates
  random_case_rates <- reactive({
    sample(0:200, 3)
  })
  
  output$case_rates_durham <- renderUI({box(
    glue("{round(threshdf[1,3],2)} per 1000"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[1,3], c(-1, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  output$case_rates_manch <- renderUI({box(
    glue("{threshdf[2,3]} per 1000"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[2,3], c(-1, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  output$case_rates_concord <- renderUI({box(
    glue("{threshdf[3,3]} per 1000"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[3,3], c(-1, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  ## pct isol
  random_pct_isol <- reactive({
    sample(0:100, 3)
  })
  
  output$pct_isol_durham <- renderUI({box(
    glue("{threshdf[1,4]}"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[1,4], c(-1, 10, 50, 90)),
    solidHeader=TRUE
  )})
  
  output$pct_isol_manch <- renderUI({box(
    glue("{threshdf[2,4]}"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[2,4], c(-1, 10, 50, 90))
  )})
  
  output$pct_isol_concord <- renderUI({box(
    glue("{threshdf[3,4]}"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[3,4], c(-1, 10, 50, 90))
  )})
  
  ## pct quar
  random_pct_quar <- reactive({
    sample(0:100, 3)
  })
  
  output$pct_quar_durham <- renderUI({box(
    glue("{threshdf[1,5]}"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[1,5], c(-1, 25, 50, 90))
  )})
  
  output$pct_quar_manch <- renderUI({box(
    glue("{threshdf[2,5]}"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[2,5], c(-1, 25, 50, 90))
  )})
  
  output$pct_quar_concord <- renderUI({box(
    glue("{threshdf[3,5]}"),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(threshdf[3,5], c(-1, 25, 50, 90))
  )})
  
  ## UNH Campus Situation -------------------------------------------------------
  
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
    
    ggplot(subset(routinetesting_campus_location, campus==campus_opt()), aes(week_start_date, cases,fill=level)) +
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
    
    ggplot(subset(routinetesting_campus_personnel, campus==campus_opt()), aes(week_start_date, cases,fill=level)) +
      geom_bar(stat="identity") +
      labs(x="Week",y="Cases", title='Total new cases diagnosed per week among students and faculty')+
      scale_fill_manual(name="", values=c('darkblue','cornflowerblue'))+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(angle=90),
            axis.ticks.x=element_blank()) +
      theme_bw()
    
  })
  
  
  #made formatted boxes for the 3 box tabs in campus tab. Please feel free to change color, size etc. according to
  #other preferences
  output$n_isol_label <- renderUI({box(
    strong("# individuals in isolation"),
    br(), 
    em("Total"),
    br(),
    #em("Total  (symptomatic)"),
    #br(),
    h4(strong(threshdf$isolated[threshdf$campus == campus_opt()])),
    width=5, 
    height=80,
    solidHeader = TRUE)})
  # TO DO: commented out symptomatic because we don't have that data yet
  
  output$n_quar <- renderUI({box(
    strong(paste0("# individuals in quarantine")), 
    br(), 
    em("Total"),
    br(),
    h4(strong(threshdf$quarantined[threshdf$campus == campus_opt()])),
    width=6,
    height=80,
    solidHeader = TRUE)})
  
  output$n_test <- renderUI({box(
    # strong("Days from test to isolation"), 
    # br(), 
    # em("7-day median"),
    # br(),
    # h4(strong(n_test_no)),
    width=1,
    height=80,
    solidHeader = TRUE)})
    # TO DO: commented out because we do not have this data yet; change widths if we begin to include this again
  
  
  # dorm table - only needed for UNH Durham currently
  # TO DO: figure out why subseting with campus_opt() did not work
  Dorm_tab <- subset(dormdf,campus=="UNH Durham")
  
  if(nrow(Dorm_tab)<1){
    # dummy data for when there are no dorms available
    # not actually shown currently, but to make sure nothing funky happens
    dorm <- campus_opt
    cases <- threshdf$cases[threshdf$campus==campus_opt()]
    rate <- threshdf$rate[threshdf$campus==campus_opt()]
    quarantined <- threshdf$quarantined[threshdf$campus==campus_opt()]
    
    Dorm_tab <- cbind(dorm,cases,rate,quarantined)
  }else{
    Dorm_tab <- Dorm_tab %>%
                ungroup() %>%
                select(dorm, cases, rate, quarantined) %>%
                mutate(rate = round(rate, 2)) %>%
                arrange(desc(cases))
    
    Dorm_tab <- as.matrix(Dorm_tab)
  }
  
  #table out to UI
  dorm_table = htmltools::withTags(table(
    style = "font-size: 100%; width: 95%; height:70%;  background-color:#FFFFFF;margin-top = -1em",
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, "Dorm"),
        th(colspan = 1, "Active Cases"),
        th(colspan = 1, "Active Cases"),
        th(colspan = 1, "# Quarantined")
      ),
      tr(
        th("(10-day total)"),
        th("per 1000 population"),
        th("from dorm")
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
    
    date_limits <- c(min(tecCampusfinal$date), max(tecCampusfinal$date))
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

    glabel <- ggplot(data=subset(pct_pos_daily,campus==campus_opt())) +
      geom_text(aes(x=date, y=2, label=pct_pos_label)) +
      geom_text(aes(x=date, y=3.5, label=n_pos)) +
      geom_text(aes(x=date, y=5, label=n_tot)) +
      scale_y_continuous(name="", breaks=c(2, 3.5, 5), labels=c("% Positive", "# Positive", "# Submitted"), limits = c(0, 5)) +
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
    
    # glabel <- ggplot(data=subset(pct_pos,campus==campus_opt())) +
    #   geom_text(aes(x=week_no, y=5, label=pct_pos_label)) +
    #   geom_text(aes(x=week_no, y=10, label=n_not_subm)) +
    #   geom_text(aes(x=week_no, y=15, label=n_tot)) +
    #   scale_y_continuous(name="", breaks=c(5, 10, 15), labels=c("% Positive", "# Not Submitted", "# Submitted")) +
    #   theme_minimal() +
    #   theme(
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(),
    #     panel.border = element_blank(),
    #     axis.line = element_blank(),
    #     axis.text.x = element_blank(),
    #     axis.text.y = element_text(size=11),
    #     axis.ticks = element_blank(),
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     plot.title = element_blank()
    #   )
    
    glabel <- ggplot_gtable(ggplot_build(glabel))
    gtest <- ggplot_gtable(ggplot_build(gtest))
    gtest$widths <-glabel$widths 
    
    grid.arrange(gtest, glabel)
    
  })
  
  
  ## Lab delay labels
  # TO DO: this is currently commented out since data is not yet available
  
  # output$lab_delay_label <- renderUI({box(
  #   width=6,
  #   height=80,
  #   p("Median days from sample collection to test result day"),
  #   solidHeader=TRUE
  # )})
  # 
  # output$lab_unh_label <- renderUI({box(
  #   h4(random_delays()[1]),
  #   em("UNH"),
  #   width=2, 
  #   height=80,
  #   solidHeader = TRUE
  # )})
  # 
  # output$lab_quest_label <- renderUI({box(
  #   h4(random_delays()[2]),
  #   em("Quest"),
  #   width=2, 
  #   height=80,
  #   solidHeader = TRUE
  # )})
  # 
  # output$lab_cmd_label <- renderUI({box(
  #   h4(random_delays()[3]),
  #   em("CMD"),
  #   width=2, 
  #   height=80,
  #   solidHeader = TRUE
  # )})
    
})
}
#)
