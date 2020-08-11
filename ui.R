function(request) {
  dashboardPage(
    title = "UNH Dashboard",
    
    dashboardHeader(
      titleWidth = 320,
      disable = FALSE,
      tags$li(
        class = "dropdown",
        tags$a(
          href = "https://coronavirus.jhu.edu/", target = "_blank",
          tags$img(src = "jhu-logo.jpg", height = 50),
          style = "padding-top: 0; padding-bottom:0"
        )
      )
    ), # end header
    
    dashboardSidebar(
      width = 320,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home"),
        menuItem(
          "UNH Campus Summary",
          tabName = "campus-summary",
          icon = icon("search")
        ),
        menuItem(
          "UNH Laboratory Testing",
          tabName = "lab-testing",
          icon = icon("home")
        ),
        menuItem(
          "NH State Summary",
          tabName = "nh-state",
          icon = icon("user")
        ),
        menuItem(
          "About",
          tabName = "about",
          icon = icon("info-circle")
        )
      )
    ), # end Sidebar
    
    dashboardBody(
      includeCSS("www/custom.css"),
      use_sever(),
      use_waiter(include_js = FALSE),
      use_hostess(),
      waiter_show_on_load(
        html = tagList(
          h2("getting everything set up..."),
          br(),
          spin_loaders(id = 11, color = "#f1c400"),
          br(), br(),
          h3("Johns Hopkins Bloomberg School of Public Health")
        ),
        color = "#002d72"
      ),
      useShinyjs(),
      extendShinyjs(text = 'shinyjs.bkg_col = function(params) {
      var defaultParams = {
      id : null,
      col : "#f1c400"
      };
      params = shinyjs.getParams(params, defaultParams);
      var el = $("#" + params.id);
      el.css("background-color", params.col);
                    }'),
      tags$script(HTML("$('body').addClass('fixed');")),
      
      tabItems(
        ## Home --------------------------------------------------------------------
        tabItem(
          tabName = "home",
          p(class = "app-title", "UNH Dashboard", align = "center"),
          p(glue(
            "Welcome to the UNH Dashboard "
          )),
          HTML(glue(
            "<ol><li> this app will do these kinds of things <li> This too!</ol>"
          ))#,
          # br(),
          # fluidRow(
          #   box(
          #     width = 6, status = "primary",
          #     textInput("name", text_q("What is your community's name?", "help/name.md")),
          #     textInput("prepared_by", text_q("Who is preparing this report?", "help/prepared_by.md"))
          #   )
          # ),
          # fluidRow(
          #   box(width = 12, status = "primary",
          #       checkboxInput("save_server",
          #                     tagList("Please check this box to save your inputs temporarily in your browser. Checking this option may enhance your user experience with this application. If you don't check this box, you could lose your work if you lose internet connectivity or disconnect from the server for other reasons. This information will not be used by our team nor shared with anyone else.",
          #                             br(), br(), "Regardless of whether you check this box, you should regularly save your inputs locally by clicking 'Save Inputs' in the Navigation pane."))
          #   ),
          #   actionButton("surv", "GET STARTED",
          #                style = "background-color: #f1c400;"
          #   )
          # )
        ), # END Home page 
        
        
        ## UNH Campus Summary -----------------------------------------------------------------
        tabItem(
          tabName = "campus-summary",
          p(class = "app-title", "UNH Campus Summary", align = "center")
        ), # END Campus Summary
        
        ## Lab Testing ---------------------------------------------------------------
        tabItem(
          tabName = "lab-testing",
          p(class = "app-title", "UNH Laboratory Testing", align = "center")
        ), # END Laboratory Testing
        
        ## UNH State Dash ---------------------------------------------------------------
        tabItem(
          tabName = "nh-state",
          p(class = "app-title", "New Hampshire State", align = "center")
        ), # END NH State 
        
        ## Dashboard ---------------------------------------------------------------
        tabItem(
          tabName = "about",
          p(class = "app-title", "About", align = "center"),
          uiOutput("prepared_by_out")
        ) # END Dashboard
        
      ) # END tabItems
    ) # END Dashboard body
  ) # END Dashboard page
}
