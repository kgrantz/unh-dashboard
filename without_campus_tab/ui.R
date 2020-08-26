#function(request) 
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
                h3("University of New Hampshire COVID-19 Dashboard", align="center"),#the html was interfering with render plot below. Need to figure out a workaround to display the data
                     column(width=6,
                            fluidRow(
                                box(status="primary",
                                    width=NULL,
                                    solidHeader = TRUE,
                                    title = "Confirmed COVID-19 Cases in UNH Community",
                                plotOutput("epi_curve_total", height=250)
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
                                box(" ",color="black",width=2),
                                box("Active Cases ", width= 2),
                                box("Case Rate", width=2),
                                box("iso beds in use", width =2),
                                box("qu beds in use", width = 2)
              ), # TO DO: names of each reopening metric
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
          )# End column 2
      )# END tabItem
      
     
      ) # END tabItems
  ) # END dashboardBody
) # END dashboardPage
#} # END function

