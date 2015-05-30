
dashboardPage(
  dashboardHeader(title = "English League"),
  dashboardSidebar(
    selectInput("team","Choose",teamOptions,selected="Liverpool"),
    
    sidebarMenu(
      menuItem("Standings", tabName = "standings"),
      menuItem("Result Matrix", tabName = "matrix")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("standings",
#               fluidRow(
#                 valueBoxOutput("rate"),
#                 valueBoxOutput("count"),
#                 valueBoxOutput("users")
#               ),
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "League Position by Year - click point for Table",
                  ggvisOutput("plot")
                  
                ),
                box(
                  width = 6, status = "info",solidHeader = TRUE,
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
      tabItem("matrix"
#               numericInput("maxrows", "Rows to show", 25),
#               verbatimTextOutput("rawtable"),
#               downloadButton("downloadCsv", "Download as CSV")
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