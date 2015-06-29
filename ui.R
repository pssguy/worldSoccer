
dashboardPage(skin="yellow",
             
  dashboardHeader(title = "World Soccer"),
  dashboardSidebar(
    
    h4("   English League"),
    hr(),
    uiOutput("a"),
    hr(),
   # selectInput("team","Choose Team",teamOptions,selected="Liverpool"),
    
    sidebarMenu(id = "sbMenu",
      menuItem("Standings", tabName = "standings"),
              # selectInput("team","Choose",teamOptions,selected="Liverpool")),
      
      menuItem("Head To Head", tabName = "matchup"),
      menuItem("Result Matrix", tabName = "matrix"),
      menuItem("Info", tabName = "info", icon = icon("info")),
      menuItem("Other Dashboards",
               menuSubItem("Bundesliga",href = "https://mytinyshinys.shinyapps.io/bundesliga"),
              # menuSubItem("English",href = "https://mytinyshinys.shinyapps.io/worldSoccer"),
               menuSubItem("Eredivise",href = "https://mytinyshinys.shinyapps.io/eredivise"),
               menuSubItem("La Liga",href = "https://mytinyshinys.shinyapps.io/laLiga"),
               menuSubItem("Serie A",href = "https://mytinyshinys.shinyapps.io/seriea")        
      ),
      menuItem("", icon = icon("twitter-square"),
               href = "https://twitter.com/pssGuy"),
      menuItem("", icon = icon("envelope"),
               href = "mailto:agcur@rogers.com")
    
  #    menuItem("mts Sites", 
  #             menuSubItem("Sports",href = "https://mytinyshinys.shinyapps.io/sports/"))
  
 #   ) 
 
  )
 ),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
    tabItems(
      tabItem("standings",

              fluidRow(
                column(width=6,
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "League Position by Year - click point for Table and Results",
                  ggvisOutput("plot")
                  
                )),
                column(width=6,
                box(
                  width = 12, status = "success",solidHeader = TRUE,
                  collapsible=TRUE, title = "League table",
                  DT::dataTableOutput('standings')
                ),
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  title = "Season Results",
                  collapsible=TRUE,
                  #collapsible=TRUE,collapsed = TRUE, bug?  
                  DT::dataTableOutput("results")
                  
                )
              )

      )
),
      tabItem("matrix",

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
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "Head To Head - click row for details against opponent",
            collapsible = TRUE,
            DT::dataTableOutput("headToHead")),
          box(
            width = 6, status = "success", solidHeader = TRUE,
            title = "All Games",
            #textOutput("check"))
            DT::dataTableOutput("HtoHGames"))
          ),
fluidRow(
  box(width=8,
    status = "success", solidHeader = TRUE,
    title = "Summary of Seasons Difference in Overall Position",
    ggvisOutput("HtoHPos"))
        )
),
tabItem("info", includeMarkdown("info.md"))
    )#tabitems
  ) #body
) #page


