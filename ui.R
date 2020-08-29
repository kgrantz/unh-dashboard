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
    #universal HTML tag to center-align all boxes
    tags$style(HTML("div{text-align: center
                                }
                     #box1{height: 60px;
                      }
                     ")),
    tabItems(
      ## Home --------------------------------------------------------------------
      tabItem(
        tabName = "dashboard",
        h3("University of New Hampshire COVID-19 Dashboard"),#the html was interfering with render plot below. Need to figure out a workaround to display the data
        column(width=6,
               fluidRow(
                 box(status="primary",
                     width=NULL,
                     solidHeader = TRUE,
                     title = "Confirmed COVID-19 Cases in UNH Community",
                     plotOutput("epi_curve_total", height=300)
                 )# end box
               ),#end fluid row
               fluidRow(
                 box(
                   status="primary",
                   solidHeader = TRUE,
                   width=NULL,
                   title = "Statewide conditions",
                   fluidRow(
                     valueBoxOutput("state_case"),
                     valueBoxOutput("hospitalization"),
                     valueBoxOutput("current_restrictions") # TO DO: generate these boxes (or box within this box) in server to control color
                     
                     
                   )#end fluid row
                 )#end box
               )#end fluid row
        ),#end Column 1
        column(
          width=6,
          box(
            status="primary",
            width=NULL,
            solidHeader=TRUE,
            title="College Operating Conditions",
           
            fluidRow(
              box(p(""),width=2, height=60, style="color: black; font-size: 14px; font-weight: bold"),
              box("Active Cases ",width=2,height=60,style="color: black; font-size: 14px; font-weight: bold"),
              box("Case Rate",width=2,height=60,style="color: black; font-size: 14px; font-weight: bold"),
              box("% iso beds in use",width=2,height=60,style="color: black; font-size: 11px; font-weight: bold"),
              box("% qu beds in use", width=2,height=60,style="color: black; font-size: 11px; font-weight: bold")
            ),#added html formatting to boxes 
            # TO DO: names of each reopening metric
            fluidRow(
              # TO DO add 4 boxes for each campus reopening status - build in server to control color
              box("Durham",background="teal",width= 2,height=60,style="color: black; font-size: 12px; font-weight: bold")
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
        )# End column 2
      ),# END tabItem
      ## Campus --------------------------------------------------------------------
      tabItem(
        tabName="campus",
        h3(textOutput("campus_text")),
        
        ## START: lefthand column/box
        
        column(width = 6,style = "background-color:#FFFFFF;",
        #Gave a uniform white background to the column. Remove if needed
          # epi curve - tabset box with tabs for different views
          fluidRow(
            tabBox(title = h4("Epidemic Curve"),
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "campus_epi_curve",width=NULL,height=250,
                   tabPanel("On / Off campus", plotOutput("location_plot",height=200)),
                   tabPanel("Student / Faculty", plotOutput("personnel_plot",height=200))
            )
            

            ),# end fluidRow
              # TO DO: add in radio buttons to choose display view for epi curve
            #)
          #),#end right side column,
          #column(width=6,
          # numeric indicators
          fluidRow(
            # TO DO: format these more nicely
              uiOutput("n_isol_label"),
            #),
          #fluidRow(
              uiOutput("n_quar"),
            #),
            #),
            #fluidRow(
              uiOutput("n_test")),
          # Dorm table 
          fluidRow(
            dataTableOutput("mytable",height=75),width=NULL,height=100)
            
          
          ),# END lefthand column
        #not many changes made in campus tab after this. Testing statistics column still pending. Need more
        #details on this plot.
        
      column(width=6,
        ## START righthand column
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
      )
      
    ) # END tabItems
  ) # END dashboardBody
#) # END dashboardPage
#} # END function




