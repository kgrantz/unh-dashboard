ui <- 
  dashboardPage(
    
    ## HEADER --------------- 
    dashboardHeader(
      title = "UNH Dashboard"
    ), # end header
    
    ## SIDEBAR --------------- 
    dashboardSidebar(
      width=240,
      sidebarMenu(
        id="tabs",
        menuItem("Dashboard", 
                 tabName = "dashboard", 
                 icon = icon("dashboard")),
        menuItem("Campus", 
                 tabName = "campus",
                 icon = icon("university"))
      ) # END sidebarMenu
    ), # END sidebar
    
    
    ## BODY --------------- 
    dashboardBody(
      
      tabItems(
        ## Home --------------------------------------------------------------------
        tabItem(
          tabName = "dashboard",
          h3(strong("University of New Hampshire COVID-19 Dashboard"), 
             align="center",
             style = "font-family:'glypha';color:#003591"),
          br(),
          textOutput("date_updated"), # TO DO: figure out why this isn't rendering
          fluidPage(
            ## START: lefthand column
            column(
              width=6,
              box(
                status="primary",
                width=NULL,
                solidHeader = TRUE,
                title = div("Confirmed COVID-19 Cases in UNH Community", style = "font-family:'Source Sans Pro'"),
                plotOutput(outputId = "epi_curve_total") # TO DO: figure out why this isn't rendering
              ),
              box(
                status="primary",
                width=NULL,
                solidHeader = TRUE,
                title = "Statewide conditions",
                fluidRow(
                  uiOutput(outputId = "state_cases"), # TO DO: figure out why this isn't rendering
                  box( p("Hospitalizations"), # TO DO: generate these boxes (or box within this box) in server to control color
                       p("14-day"),
                       background = "blue",
                       width=4),
                  box( p("Current Restrictions"), # TO DO: generate these boxes (or box within this box) in server to control color
                       br(),
                       background = "blue",
                       width=4)
                ) 
              )
            ), # END lefthand column
            
            ## START: righthand column
            column(
              width=6,
              box(
                status="primary",
                width=NULL,
                solidHeader=TRUE,
                title="College Operating Conditions",
                fluidRow(), # TO DO: names of each reopening metric
                fluidRow(
                  # TO DO add 4 boxes for each campus reopening status - build in server to control color
                  box("Durham", width=2)
                  #box1
                  #box2
                  #box3
                  #box4
                ),
                
                fluidRow("Manchester"),
                
                fluidRow("Concord (Law)"),
                
                fluidRow("Keene State"),
                
                fluidRow("Plymouth St.")
              )
            ) # END righthand column
            
          ) # END fluidPage
        ), # END home page
        
        ## Campus --------------------------------------------------------------------
        tabItem(
          tabName="campus",
          
          fluidRow(
            column(
              width=6, 
              offset=3,
              selectInput("campus", 
                          "Select a campus to display:", 
                          choices = c("All",
                                      "Durham",
                                      "Manchester",
                                      "Concord",
                                      "Keene State",
                                      "Plymouth State"),
                          selected = "All")
            )
          ),
          
          ## START: lefthand column/box
          box(
            width=6,
            status="primary",
            solidHeader = TRUE,
            title = uiOutput(outputId="epi_curve_detail_title"), # TO DO: figure out why this isn't rendering as UI or text
            
            # epi curve
            fluidRow(
              column(
                width=9,
                plotOutput(outputId="epi_curve_detail")
              ),
              column(
                width=3,
                radioButtons("epicurve_var",
                             "Show cases by:",
                              choices=c("Student/Faculty + Staff"=1, "Off/On-Campus"=2),
                              selected=1)
                # TO DO: add in radio buttons to choose display view for epi curve
              )
            ),
            
            # numeric indicators
            fluidRow(
              # TO DO: format these more nicely
              box(
                p("# active cases in isolation"),
                p("total (symptomatic)"),
                uiOutput("n_isol_label"),
                background = "blue",
                width=4
              ),
              box(
                p("# quarantined"),
                p("total"),
                uiOutput("n_quar"),
                background = "blue",
                width=4
              ),
              box(
                p("Days from test to isolation"),
                p("7-day median"),
                uiOutput("n_isol_label"), # TO DO: make days
                background = "blue",
                width=4
              )
            ),
            
            # Dorm table 
            fluidRow(
              # TO DO - build dorm table as kable object in server + output here
            )
            
          ), # END lefthand column
          
          ## START righthand column
          box(
            width=6,
            status="primary",
            solidHeader = TRUE,
            title = "Testing Statistics",
            
            fluidRow(
              # TO DO: add in testing figure; build in "tests not submitted" to ggplot object
            ),
            
            fluidRow(
              box(
                width=6,
                p("Median days from sample collection to test result day")
              ),
              box(
                width=2,
                status="primary",
                solidHeader = TRUE,
                title = "UNH"
                # TO DO: add in UNH sample dates
              ),
              box(
                width=2,
                status="primary",
                solidHeader = TRUE,
                title = "Quest"
                # TO DO: add in Quest testing delays
              ),
              box(
                width=2,
                status="primary",
                solidHeader = TRUE,
                title = "CMD"
                # TO DO: add in CMD testing delays
              )
              
            )
          ) # END righthand column
          
        ) # END campus page

      ) # END tabItems
    ) # END dashboardBody
) # END dashboardPage

  
  