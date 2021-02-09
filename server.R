#shinyServer(

function(input, output, session) {
  library(tidyverse)
  library(DT)
  library(htmltools)
  library(gridExtra)
  #library(cowplot)
 
  
  
  cdata <- session$clientData
  
  observeEvent(input$InputDate,{ 

  ##getting selected date from UI. Default is last date with available data  
  
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
  
  week_breaks <-  unique(epi_curve_overall_week$week_start_date)
  
  biweek_breaks <- week_breaks[length(week_breaks)-seq(0,length(week_breaks),by=2)]
  
  
  output$epi_curve_total <- renderPlot({
    p = ggplot(epi_curve_overall_week, aes(week_start_date, cases)) +
          geom_bar(stat="identity", fill="darkblue") +
          labs(x="Week",y="Cases", title='Total new cases diagnosed per week') +
          scale_x_date(breaks = week_breaks, date_labels = "%b-%d") +
          theme_bw()+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_text(angle=90),
                axis.ticks.x=element_blank()) 
    p
  })
  
  
  output$epi_curve_total_daily <- renderPlot({
    p = ggplot(epi_curve_overall_day, aes(date, cases)) +
      geom_bar(stat="identity", fill="darkblue") +
      labs(x="Date",y="Cases", title='Total new cases diagnosed per day') +
      scale_x_date(breaks = biweek_breaks, date_labels = "%b-%d",
                   minor_breaks = week_breaks) +
      theme_bw()
      # theme(axis.title.x=element_blank(),
      #       axis.text.x=element_text(angle=90),
      #       axis.ticks.x=element_blank()) 
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
    background=pick_color_threshold_numeric(as.numeric(state_curr_cases), c(0, 100, 3000, 6000)),
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
    background=pick_color_threshold_numeric(as.numeric(state_curr_hosp), c(0, 25, 200, 400)),
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
 
   url <- a("Statewide restrictions", href="https://www.covidguidance.nh.gov/")
  output$current_restrictions <- renderUI({box(
    tagList(url),
    width=4
  )})
  
  output$state_date_updated <- renderText({statedatetimeupdated})
  
  ## CAMPUS SITUATION ---------
  
  ## active cases
  output$active_cases_durham <- renderUI({box(
    subset(threshdf, campus == "UNH DURHAM")$cases,
    width=3, 
    height=80,
    background=pick_color_threshold_numeric(subset(threshdf, campus == "UNH DURHAM")$cases, c(-1, 20, 150, 300)),
    solidHeader = TRUE
  )})
  
  output$active_cases_manch <- renderUI({box(
    subset(threshdf, campus == "UNH MANCHESTER")$cases,
    width=3,
    height=80,
    background=pick_color_threshold_numeric(subset(threshdf, campus == "UNH MANCHESTER")$cases, c(-1, 20, 150, 300)),
    solidHeader=TRUE
  )})
  
  output$active_cases_concord <- renderUI({box(
    subset(threshdf, campus == "UNH LAW")$cases,
    width=3,
    height=80,
    background=pick_color_threshold_numeric(subset(threshdf, campus == "UNH LAW")$cases, c(-1, 20, 150, 300)),
    solidHeader=TRUE
  )})
  
  ## case rates ---
  output$case_rates_durham <- renderUI({box(
    paste(round(subset(threshdf, campus == "UNH DURHAM")$rate,2)," per 1000", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(subset(threshdf, campus == "UNH DURHAM")$rate, c(-1, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  output$case_rates_manch <- renderUI({box(
    paste(round(subset(threshdf, campus == "UNH MANCHESTER")$rate,2)," per 1000", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(subset(threshdf, campus == "UNH MANCHESTER")$rate, c(-1, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  output$case_rates_concord <- renderUI({box(
    paste(round(subset(threshdf, campus == "UNH LAW")$rate,2)," per 1000", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(subset(threshdf, campus == "UNH LAW")$rate, c(-1, 5, 100, 200)),
    solidHeader=TRUE
  )})
  
  ## 7-day % averages ---
  pos_date <- max(pct_pos_daily$date)
  dur_pct <- subset(pct_pos_daily, campus == "UNH DURHAM" & date==pos_date & level=="Total")$pct_pos_wk
  manch_pct <- subset(pct_pos_daily, campus == "UNH MANCHESTER" & date==pos_date & level=="Total")$pct_pos_wk
  conc_pct <- subset(pct_pos_daily, campus == "UNH LAW" & date==pos_date & level=="Total")$pct_pos_wk
  
  output$pct_pos_durham <- renderUI({box(
    paste(round(dur_pct*100,1), "%", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(dur_pct, c(-1, 0.0025, 0.01, 0.07)),
    solidHeader=TRUE
  )})
  
  output$pct_pos_manch <- renderUI({box(
    paste(round(manch_pct*100,1), "%", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(manch_pct, c(-1, 0.0025, 0.01, 0.07)),
    solidHeader=TRUE
  )})
  
  output$pct_pos_concord <- renderUI({box(
    paste(round(conc_pct*100,1), "%", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(conc_pct, c(-1, 0.0025, 0.01, 0.07)),
    solidHeader=TRUE
  )})
  
  ## pct isol ---
  pct_isol_dur <- subset(threshdf, campus == "UNH DURHAM")$isolated_in_bed/217 * 100
  
  output$pct_isol_durham <- renderUI({box(
    paste(round(pct_isol_dur, 1), "%", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(pct_isol_dur, c(-1, 10, 50, 90)),
    solidHeader=TRUE
  )})
  
  output$pct_isol_manch <- renderUI({box(
    em("No isolation beds", style = "font-size:9pt;"),
    width=3,
    height=80,
    background = NULL
  )})

  output$pct_isol_concord <- renderUI({box(
    em("No isolation beds", style = "font-size:9pt;"),
    width=3,
    height=80,
    background = NULL
  )})
  
  ## pct quar ---
  pct_quar_dur <- subset(threshdf, campus == "UNH DURHAM")$quarantined_in_bed/180 * 100
  
  output$pct_quar_durham <- renderUI({box(
    paste(round(pct_quar_dur, 1), "%", sep=""),
    width=3,
    height=80,
    background=pick_color_threshold_numeric(pct_quar_dur, c(-1, 25, 50, 90)),
    solidHeader=TRUE
  )})
  
  output$pct_quar_manch <- renderUI({box(
    em("No quarantine beds", style = "font-size:9pt;"),
    width=3,
    height=80,
    background = NULL
  )})

  output$pct_quar_concord <- renderUI({box(
    em("No quarantine beds", style = "font-size:9pt;"),
    width=3,
    height=80,
    background = NULL
  )})
  
  ## UNH Campus Situation -------------------------------------------------------
  
  #dynamic sidebar menu conditional on selecting campus tab. Wrote a render function to reduce UI clutter.
  output$campus_dropdown <- renderMenu(selectInput("Campus", label="Select:", 
                                                   choices = c(unique(tecCampusfinal$campus)), 
                                                   selected = "UNH DURHAM")
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
      scale_x_date(breaks = week_breaks, date_labels = "%b-%d") +
      theme_bw()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(angle=90),
            axis.ticks.x=element_blank()) 
    
  })
  
  
  #plot for student/faculty split
  output$personnel_plot <- renderPlot({
    
    ggplot(subset(routinetesting_campus_personnel, campus==campus_opt()), aes(week_start_date, cases,fill=level)) +
      geom_bar(stat="identity") +
      labs(x="Week",y="Cases", title='Total new cases diagnosed per week among students and faculty')+
      scale_fill_manual(name="", values=c('darkblue','cornflowerblue'))+
      scale_x_date(breaks = week_breaks, date_labels = "%b-%d") +
      theme_bw()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_text(angle=90),
            axis.ticks.x=element_blank()) 
    
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
    width=6, 
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
  Dorm_tab <- subset(dormdf,campus=="UNH DURHAM")
  
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
                select(dorm, cases, quarantined)
    
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
        #th(colspan = 1, "Active Cases"),
        th(colspan = 1, "# Quarantined")
      ),
      tr(
        th("(10-day total)"),
        #th("per 1000 population"),
        th("from dorm")
      )
      
    )
  ))
  
  output$mytable = renderDataTable({
    DT::datatable(
      Dorm_tab,
      options = list(dom="tp", pageLength=15),
      container = dorm_table
    )
  })
  
  
  ## Lab Testing --------------------------------------------------------
  
  ## Lab testing plot/numbers
  # TO DO (lower priority) -- add hover to the plot for the daily counts of each category
  
  tecCampusfinal$result <- factor(tecCampusfinal$result, levels=c("Positive", "Negative", "Invalid / Rejected / Not Performed"))
  
  ##Changing pct pos label to improve readability  

    pct_pos_daily$pct_pos_label2 <- paste(ifelse(pct_pos_daily$pct_pos_day ==0 , 0, ifelse(pct_pos_daily$pct_pos_day < 0.001,"<0.1",as.character(round(pct_pos_daily$pct_pos_day*100,1)))),"%",sep="")
  
  
  pct_pos_daily <- pct_pos_daily %>%
  mutate("7 day % positive" = paste(round(pct_pos_wk,5)*100,"%",sep=""),"% Positive" = pct_pos_label2,"# Positive" = n_pos_day, "# Submitted" = n_tot_day)
  
  
  ## merging campus table w pct_pos table to enable visualization in same plot
  tecCampusfinal_w_pct <- merge(tecCampusfinal, pct_pos_daily, all.x=TRUE)
  
  
  ay <- list(
    titlefont=list(color="#de2d26", size = "12"),
    tickfont = list(color = "#de2d26", size = "12"),
    overlaying = "y",
    side = "right",
    title = "7 day % positive",
    ticksuffix = "%"
  )

  date_breaks = seq(min(tecCampusfinal$date),max(tecCampusfinal$date),by=1)
  
    

    output$testing_plot <- renderPlotly({
      
      ##getting range of y - axis for bar chart 
      ylim.prim <- c(0,max(subset(tecCampusfinal,campus == campus_opt() & level=="Total" &!is.na(result))$tests))   # in this example, precipitation
      ## y -axis rane for % pos trend line
      ylim.sec <- c(0,0.05)   
      
      ## getting scaling factors
      b <- diff(ylim.prim)/diff(ylim.sec)
      a <- b*(ylim.prim[1] - ylim.sec[1])
      
             ##subsetting table for required levels plus adding labels for plotly
      gtest<-ggplot(subset(tecCampusfinal_w_pct,campus==campus_opt() & !is.na(result) & level=="Total"), aes(label1=`# Positive`,label2 = `# Submitted`)) +
             ##%positive label for bar plot along with bars
             geom_bar(aes(date, tests,fill=result, label=`% Positive`),stat="identity",width=0.4) +
             ## 7 day % positive line w label
             geom_line(aes(x=date,y = a + pct_pos_wk*b, label=`7 day % positive`), color = "#de2d26", size=1.2) +
             #geom_bar(data=subset(tecCampusfinal,campus==campus_opt() & !is.na(result) & level=="Total"), aes(x=date, y=tests, fill=result), stat="identity",width=0.4) +
             scale_fill_manual(name="", values=c("#bbc1c9","#5c7596","#636363"))+
             scale_x_date(name="", breaks = date_breaks, date_labels = "%b-%d") +
      scale_y_continuous(name="", sec.axis = sec_axis(~ (./b), name = "7 -day %positive")) +
      theme_minimal() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle=90),
            legend.position = "bottom")
  
      ggplotly(gtest, 
               width=cdata$output_testing_plot_width*0.99,
               #selecting which numbers to display in pop -up
               tooltip = c("label","label1","label2"))%>%
        ##adding secondary y axis
        add_lines(x=Sys.Date,y=c(0,5),colors = NULL, yaxis="y2", 
                  showlegend=FALSE, inherit=FALSE) %>%
        layout(yaxis2 = ay) %>%
        layout(legend = list(
          orientation = "h", x = 1.1, y = 1.3)) %>%
        config(displayModeBar = F)
      
  })


  output$testing_plot_student <- renderPlotly({
    ylim.prim <- c(0,max(subset(tecCampusfinal,campus == campus_opt() & level=="Student" &!is.na(result))$tests))   # in this example, precipitation
    ylim.sec <- c(0,0.05)    # in this example, temperature
    
    b <- diff(ylim.prim)/diff(ylim.sec)
    a <- b*(ylim.prim[1] - ylim.sec[1])
    
    ##subsetting table for required levels plus adding labels for plotly
    gtest <- ggplot(subset(tecCampusfinal_w_pct,campus==campus_opt() & !is.na(result) & level=="Student"), aes(label1=`# Positive`,label2 = `# Submitted`)) +
      ##%positive label for bar plot along with bars
      geom_bar(aes(date, tests,fill=result, label=`% Positive`),stat="identity",width=0.4) +
      ## 7 day % positive line w label
      geom_line(aes(x=date,y = a + pct_pos_wk*b, label=`7 day % positive`), color = "#de2d26", size=1.2) +
      #geom_bar(data=subset(tecCampusfinal,campus==campus_opt() & !is.na(result) & level=="Total"), aes(x=date, y=tests, fill=result), stat="identity",width=0.4) +
      scale_fill_manual(name="", values=c("#bbc1c9","#5c7596","#636363"))+
      scale_x_date(name="", breaks = date_breaks, date_labels = "%b-%d") +
      scale_y_continuous(name="", sec.axis = sec_axis(~ (./b), name = "7 -day %positive")) +
      scale_y_continuous(name="",breaks = int_breaks, sec.axis = sec_axis(~ (./b), name = "7 -day %positive")) +

      theme_minimal() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle=90),
            legend.position = "bottom")
    
    ##fixing the width of plotly figure in a flexible way
    ggplotly(gtest, 
             width=cdata$output_testing_plot_student_width*0.99,
             #selecting which numbers to display in pop -up
             tooltip = c("label","label1","label2"))%>%
      ##adding secondary y axis
            add_lines(x=Sys.Date,y=c(0,5),colors = NULL, yaxis="y2", 
                      showlegend=FALSE, inherit=FALSE) %>%
            layout(yaxis2 = ay) %>%
            layout(legend = list(
                  orientation = "h", x = 1.1, y = 1.3)) %>%
      config(displayModeBar = F)

    
  })


  output$testing_plot_faculty <- renderPlotly({
    ylim.prim <- c(0,max(subset(tecCampusfinal,campus == campus_opt() & level=="Faculty/Staff" &!is.na(result))$tests))   # in this example, precipitation
    ylim.sec <- c(0,0.05)    # in this example, temperature
    
    b <- diff(ylim.prim)/diff(ylim.sec)
    a <- b*(ylim.prim[1] - ylim.sec[1])

    
    ##subsetting table for required levels plus adding labels for plotly
    gtest <- ggplot(subset(tecCampusfinal_w_pct,campus==campus_opt() & !is.na(result) & level=="Faculty/Staff"), aes(label1=`# Positive`,label2 = `# Submitted`)) +
              ##%positive label for bar plot along with bars
              geom_bar(aes(date, tests,fill=result, label=`% Positive`),stat="identity",width=0.4) +
              ## 7 day % positive line w label
              geom_line(aes(x=date,y = a + pct_pos_wk*b, label=`7 day % positive`), color = "#de2d26", size=1.2) +
              #geom_bar(data=subset(tecCampusfinal,campus==campus_opt() & !is.na(result) & level=="Total"), aes(x=date, y=tests, fill=result), stat="identity",width=0.4) +
              scale_fill_manual(name="", values=c("#bbc1c9","#5c7596","#636363"))+
              scale_x_date(name="", breaks = date_breaks, date_labels = "%b-%d") +
              scale_y_continuous(name="", sec.axis = sec_axis(~ (./b), name = "7 -day %positive")) +
              theme_minimal() +
              theme(axis.ticks.x=element_blank(),
                    axis.text.x=element_text(angle=90),
                    legend.position = "bottom")

    ##fixing the width of plotly figure in a flexible way
    ggplotly(gtest, 
             width=cdata$output_testing_plot_faculty_width*0.99,
             #selecting which numbers to display in pop -up
             tooltip = c("label","label1","label2"))%>%
      ##adding secondary y axis
            add_lines(x=Sys.Date,y=c(0,5),colors = NULL, yaxis="y2", 
                      showlegend=FALSE, inherit=FALSE) %>%
            layout(yaxis2 = ay) %>%
            layout(legend = list(
                    orientation = "h", x = 1.1, y = 1.3)) %>%
      config(displayModeBar = F)
    
  })

  #table out to UI
  week_lab_cont = htmltools::withTags(table(
    style = "font-size: 95%; width: 85%; height:80%;  background-color:#FFFFFF;margin-top = -1em",
    class = 'display',
    thead(
      tr(
        th(colspan = 1, " "),
        th(colspan = 1, "Tests submitted"),
        th(colspan = 1, "Unique IDs\nsubmitting test"),
        th(colspan = 1, "Tests submitted\nper unique ID "),
        th(colspan = 1, "Valid Results")
      )
    )
  ))

  lab_weekly_table <- lab_weekly_table %>% arrange(desc(n_test))
  
  output$lab_table = renderDataTable({
    DT::datatable(
      subset(lab_weekly_table, campus == campus_opt())[ ,c("level","n_test","n_ppl_tested","n_test_per_pers", "n_valid_res")],
      options = list(dom="t", ordering=F),
      container = week_lab_cont,
      rownames=FALSE)
  })
  
      
})
}
#)
