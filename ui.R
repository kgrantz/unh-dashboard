function(request) {
  dashboardPage(
    
    ## HEADER --------------- 
    dashboardHeader(
      title = "UNH Dashboard"
    ), # end header
    
    ## SIDEBAR --------------- 
    dashboardSidebar(
      width=280,
      sidebarMenu(
        id="tabs",
        menuItem("Dashboard", 
                 tabName = "dashboard", 
                 icon = icon("dashboard")),
        menuItem("Campus", 
                 tabName = "campus",
                 icon = icon("university")),
        menuItem("Laboratory Testing",
                 tabName = "lab",
                 icon=icon("vial")),
        menuItem("NH State Summary",
                 tabName = "nh-state",
                 icon = icon("globe"))
      ) # END sidebarMenu
    ), # END sideBAR
    
    
    ## BODY --------------- 
    dashboardBody(
      includeCSS("www/custom.css"),
      use_sever(),
      use_waiter(include_js = FALSE),
      use_hostess(),
      waiter_show_on_load(
        html = tagList(
          h2("loading..."),
          br(),
          spin_loaders(id = 11, color = "#f77a05"),
          br(), br(),
          h3("University of New Hampshire")
        ),
        color = "#0044bb"
      ),
      useShinyjs(),
      extendShinyjs(text = 'shinyjs.bkg_col = function(params) {
      var defaultParams = {
      id : null,
      col : "#001d52"
      };
      params = shinyjs.getParams(params, defaultParams);
      var el = $("#" + params.id);
      el.css("background-color", params.col);
                    }'),
      tags$script(HTML("$('body').addClass('fixed');")),
      
      tabItems(
        ## Home --------------------------------------------------------------------
        tabItem(
          tabName = "dashboard",
          p(class="app-title", "UNH Dashboard", align = "center"),
          p(glue(
            "Welcome to the UNH Dashboard "
          )),
          HTML(glue(
            "<ol><li> This app will do these kinds of things",
            "<li> This too!</ol>"
          ))
        ), # END Home page   
        
        ## Campus --------------------------------------------------------------------
        tabItem(
          tabName="campus"
        ),
        
        ## Lab testing --------------------------------------------------------------------
        tabItem(
          tabName="lab"
        ),        
     
        ## NH State --------------------------------------------------------------------
        tabItem(
          tabName="nh-state"
        )
      ) # END tabItems
    ) # END dashboardBody
  ) # END dashboardPage
} # END function
  
  
  