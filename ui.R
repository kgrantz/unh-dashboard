#function(request) 
dashboardPage(
  
  
  
  # HEADER --------------- 
  dashboardHeader(
    title = "UNH Dashboard"
  ), # end header
  
  ## SIDEBAR --------------- 
  ##added conditional dropdown for campus tab
  dashboardSidebar(
    width=240,
    sidebarMenu(
      id="tabs",
      menuItem("Dashboard", 
               tabName = "dashboard", 
               icon = icon("dashboard")),
      menuItem("Campus", 
               tabName = "campus",
               icon = icon("university")),
      conditionalPanel(
        'input.tabs == "campus"',
        menuItemOutput("campus_dropdown")
      )
      
    ) # END sidebarMenu
  ), # END sidebar
  
  ## BODY --------------- 
  dashboardBody(
    #universal HTML tag to center-align all boxes, remove shadow borders
    tags$style(HTML("div{text-align: center
                                }
                     #box1{height: 60px;
                     }
                    .box{-webkit-box-shadow: none; 
                    -moz-box-shadow: none;
                    box-shadow: none;}
                     #mytable {
                     width: 80px;
                     }
                     ")),
    tabItems(
      ## Home --------------------------------------------------------------------
      tabItem(
        tabName = "dashboard",
        div(style = "margin-top:-1em",  ##shaving off annoying blank space from the top
            h4(strong("University of New Hampshire COVID-19 Dashboard"))#the html was interfering with render plot below. Need to figure out a workaround to display the data
        ),
        column(width=6,
               fluidRow(
                 box(status="primary",
                     width=NULL,
                     solidHeader = TRUE,
                     title = "Confirmed COVID-19 Cases in UNH Community",
                     plotOutput("epi_curve_total", height=248)
                 )# end box
               ),#end fluid row
               fluidRow(
                 box(
                   status="primary",
                   solidHeader = TRUE,
                   width=NULL,
                   title = "Statewide conditions",
                   fluidRow(
                     uiOutput("state_case_label"),
                     uiOutput("hospitalization_label"), 
                     uiOutput("current_restrictions_label")
                   ),
                   fluidRow(
                     uiOutput("state_case"),
                     uiOutput("hospitalization"),
                     uiOutput("current_restrictions")
                   )#end fluid row
                 )#end box
               )#end fluid row
        ),#end Column 1
        
        column(
          width=6,
          div(style = "font-size: 16px; vertical-align: middle",
              box(
                status="primary",
                width=NULL,
                solidHeader=TRUE,
                title="College Operating Conditions",
                div(style = "margin-top:0.04em;",
                    fluidRow(
                      box("",width=3,height=30, style="font-weight: bold",solidHeader=TRUE),
                      box("Durham",width=3,height=30,style="font-weight: bold; font-size: 13px",solidHeader=TRUE),
                      box("Manchester",width=3,height=30,style="font-weight: bold;font-size: 13px",solidHeader=TRUE),
                      box("Concord",width=3,height=30,style="font-weight: bold;font-size: 13px",solidHeader=TRUE),
                    )
                ),
                fluidRow(
                  box("Active Cases",width=3, height=80,style="font-weight: bold; font-size: 13px; vertical-align: middle",solidHeader=TRUE),
                  uiOutput("active_cases_durham"),
                  uiOutput("active_cases_manch"),
                  uiOutput("active_cases_concord")
                ),
                fluidRow(
                  box("Case Rate",width=3, height=80,style="font-weight: bold; font-size: 13px; vertical-align: middle",solidHeader=TRUE),
                  uiOutput("case_rates_durham"),
                  uiOutput("case_rates_manch"),
                  uiOutput("case_rates_concord")
                ),
                fluidRow(
                  box("% Isolation", br(), "Beds in Use",width=3, height=80,style="font-weight: bold; font-size: 13px; vertical-align: middle",solidHeader=TRUE),
                  uiOutput("pct_isol_durham"),
                  uiOutput("pct_isol_manch"),
                  uiOutput("pct_isol_concord")
                ),
                fluidRow(
                  box("% Quar.", br(), "Beds in Use",width=3, height=80,style="font-weight: bold; font-size: 13px; vertical-align: middle",solidHeader=TRUE),
                  uiOutput("pct_quar_durham"),
                  uiOutput("pct_quar_manch"),
                  uiOutput("pct_quar_concord")
                )
              ) # end box
          ) # end div
        )# End column 2
      ),# END tabItem
      
      ## Campus --------------------------------------------------------------------
      tabItem(
        tabName="campus", 
        div(style = "margin-top:-1em", #the html was interfering with render plot below. Need to figure out a workaround to display the data
            
            h4(strong(textOutput("campus_text")))
        ),
        
        ## START: lefthand column/box
        
        column(width = 6,
               style = "background-color:#FFFFFF;", #Gave a uniform white background to the column. Remove if needed
               height=NULL,
               
               # epi curve - tabset box with tabs for different views
               fluidRow(
                 tabBox(title = h5("Epidemic Curve"),
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "campus_epi_curve",
                        width=NULL,
                        height=240,
                        tabPanel("On / Off campus", plotOutput("location_plot",height=200)),
                        tabPanel("Student / Faculty", plotOutput("personnel_plot",height=200))
                 )
                 
               ),# end fluidRow
               
               # numeric indicators
               # TO DO: figure out how to remove gray bars?
               div(style = "font-size: 13px;",##reducing font size to fit things in
                   fluidRow(
                     uiOutput("n_isol_label"),
                     #),
                     #fluidRow(
                     uiOutput("n_quar"),
                     #),
                     #),
                     #fluidRow(
                     uiOutput("n_test"))
               ), # end fluidRow
               
               
               # Dorm table 
               fluidRow(
                 div(style = "font-size: 12px;", ##reducing font size.
                     fluidRow(
                       dataTableOutput("mytable"),
                       width=NULL,
                       height=70)
                 )
               ) # end fluidRow
               
        ),# END lefthand column
        
        column(width=6, ## START righthand column
               box(
                 status="primary",
                 width=NULL,
                 solidHeader = TRUE,
                 title = "Testing Statistics",
                 plotOutput("testing_plot"), # TO DO: fix dimensions to server dimensions if needed
                 br(),
                 fluidRow(
                   box(
                     width=6,
                     height=80,
                     p("Median days from sample collection to test result day"),
                     solidHeader=TRUE
                   ),
                   uiOutput("lab_unh_label"), # TO DO: these aren't displaying
                   uiOutput("lab_quest_label"),
                   uiOutput("lab_cmd_label")
                 )
               ) # END box
        ) # END righthand column
        
      ) # END campus page
    )
    
  ) # END tabItems
) # END dashboardBody
#) # END dashboardPage
#} # END function




