
dashboardPage(
  dashboardHeader(title = "English League"),
  dashboardSidebar(
    #selectInput("team","Choose",teamOptions,selected="Liverpool"),
    
    sidebarMenu(
      menuItem("Standings", tabName = "standings"),
              # selectInput("team","Choose",teamOptions,selected="Liverpool")),
      menuItem("Result Matrix", tabName = "matrix"),
      menuItem("Head To Head", tabName = "matchup"),
     menuItem(""),
      menuItem("mts Sites", 
               menuSubItem("Sports",href = "https://mytinyshinys.shinyapps.io/sports/"))
      
    ) 
  ),
  dashboardBody(
    tabItems(
      tabItem("standings",
              fluidRow(
                box(
                  width = 4,
                  title = "Introduction", status = "info", solidHeader = TRUE,
                  helpText(h4("some text mmmmmm
                               mmmmmmmmm mmmmmmm mmmmmmmm mmmmmmm"))
                ),
                box(
                  width = 2,offset=1,
                  title = "Select Team", status = "warning", solidHeader = TRUE,
                  selectInput("team","",teamOptions,selected="Liverpool")
                )
                
              ),
              fluidRow(
                box(
                  width = 6, status = "success", solidHeader = TRUE,
                  title = "League Position by Year - click point for Table",
                  ggvisOutput("plot")
                  
                ),
                box(
                  width = 6, status = "success",solidHeader = TRUE,
                  title = "League table",
                  DT::dataTableOutput('standings', width = "90%")
                )
              ),
              fluidRow(
               box(
                 width = 6, status = "info", solidHeader = TRUE,
               title = "Season Results",
               DT::dataTableOutput("results")
    
  )#,
#   box(
#     width = 6, status = "info",
#     title = "League table",
#     DT::dataTableOutput('standings', width = "90%")
#   )
)
      ),
      tabItem("matrix",
#                sliderInput("year_matrix", "Choose Season", min=1870,max=2014,value=2014),
#                selectInput("division","Choose division",c("1","2")) # this needs to be a uioutput
#               downloadButton("downloadCsv", "Download as CSV")
          fluidRow(
#             column(width=5,offset=1,sliderInput("year_matrix", "Choose Season (start year)", min=1870,max=2014,value=2014,sep="")),
#             column(width=6,selectInput("division","Choose division",c("1","2")))
            column(width=10,offset=1,
                   box(width=12,
                   status="warning",solidHeader = TRUE,title="Select",
                   sliderInput("year_matrix", "Choose Season (start year)", min=1870,max=2014,value=2014,sep=""),
                   selectInput("division","Choose division",c("1","2"))
            )
            )
          ),
          fluidRow(
            column(width=10,offset=1,
                   box(width=12,
              status = "success", solidHeader = TRUE,
              title = "Match Matrix - Ordered Vertically by Final Position ",
              plotOutput("plot_matrix")
          )
            )
          )
          
  


      ),
tabItem("matchup",
        fluidRow(
          #                 valueBoxOutput("rate"),
          #                 valueBoxOutput("count"),
          selectInput("team_MU","Choose",teamOptions,selected="Arsenal")
        ),
        fluidRow(
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Head To Head - click row for details",
            DT::dataTableOutput("headToHead")),
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "All Games",
            #textOutput("check"))
            DT::dataTableOutput("HtoHGames"))
          )
        )
)
    )
  )


## original https://github.com/rstudio/shiny-examples/blob/master/087-crandash/ui.R

# dashboardPage(
#   dashboardHeader(title = "cran.rstudio.com"),
#   dashboardSidebar(
#     sliderInput("rateThreshold", "Warn when rate exceeds",
#                 min = 0, max = 50, value = 3, step = 0.1
#     ),
#     sidebarMenu(
#       menuItem("Dashboard", tabName = "dashboard"),
#       menuItem("Raw data", tabName = "rawdata")
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem("dashboard",
#               fluidRow(
#                 valueBoxOutput("rate"),
#                 valueBoxOutput("count"),
#                 valueBoxOutput("users")
#               ),
#               fluidRow(
#                 box(
#                   width = 8, status = "info", solidHeader = TRUE,
#                   title = "Popularity by package (last 5 min)",
#                   bubblesOutput("packagePlot", width = "100%", height = 600)
#                 ),
#                 box(
#                   width = 4, status = "info",
#                   title = "Top packages (last 5 min)",
#                   tableOutput("packageTable")
#                 )
#               )
#       ),
#       tabItem("rawdata",
#               numericInput("maxrows", "Rows to show", 25),
#               verbatimTextOutput("rawtable"),
#               downloadButton("downloadCsv", "Download as CSV")
#       )
#     )
#   )
# )