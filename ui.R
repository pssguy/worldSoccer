

dashboardPage(
  skin = "yellow",
  
  dashboardHeader(title = "English League"),
  dashboardSidebar(
    includeCSS("custom.css"),
    #     h4("   English League"),
    #     hr(),
    uiOutput("a"),
    #    hr(),
    # selectInput("team","Choose Team",teamOptions,selected="Liverpool"),
    
    sidebarMenu(
      id = "sbMenu",
      #       menuItem("English",
      #                menuSubItem("Standings",tabName="standingsE")),
      #
      
      #menuItem("Standings", tabName = "standings"),
      
      
      #menuItem("Head To Head", tabName = "matchup"),
      menuItem("PFA Awards", tabName = "pfaPlayers"),
      menuItem(
        "Team History",
        menuSubItem("Head To Head", tabName = "matchup"),
        menuSubItem("Result Matrix", tabName = "matrix"),
        menuSubItem("Standings", tabName = "standings", selected=TRUE)
      ),
      # menuItem("Result Matrix", tabName = "matrix"),
      # menuItem("Info", tabName = "info", icon = icon("info")),
      menuItem(
        "Other Leagues",
        menuSubItem("Bundesliga",href = "https://mytinyshinys.shinyapps.io/bundesliga"),
        # menuSubItem("English",href = "https://mytinyshinys.shinyapps.io/worldSoccer"),
        menuSubItem("Eredivise",href = "https://mytinyshinys.shinyapps.io/eredivise"),
        menuSubItem("La Liga",href = "https://mytinyshinys.shinyapps.io/laLiga"),
        menuSubItem("Serie A",href = "https://mytinyshinys.shinyapps.io/seriea")
      ),
      menuItem(
        "Other Dashboards",
        menuSubItem("Climate",href = "https://mytinyshinys.shinyapps.io/climate"),
        menuSubItem("Cricket",href = "https://mytinyshinys.shinyapps.io/cricket"),
        menuSubItem("MainlyMaps",href = "https://mytinyshinys.shinyapps.io/mainlyMaps"),
        menuSubItem("MLB",href = "https://mytinyshinys.shinyapps.io/mlbCharts"),
        
        menuSubItem("WikiGuardian",href = "https://mytinyshinys.shinyapps.io/wikiGuardian")
      ),
      
      menuItem("Info",tabName = "info",icon = icon("info")),
      
      menuItem("", icon = icon("twitter-square"),
               href = "https://twitter.com/pssGuy"),
      menuItem("", icon = icon("envelope"),
               href = "mailto:agcur@rogers.com")
      
      #    menuItem("mts Sites",
      #             menuSubItem("Sports",href = "https://mytinyshinys.shinyapps.io/sports/"))
      
      #   )
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem("standings",
              
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = 12, status = "success", solidHeader = TRUE,
                    title = "League Position by Year - click point for Table and Results",
                    ggvisOutput("plot")
                    
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = 12, status = "success",solidHeader = TRUE,
                    collapsible = TRUE, title = "League table",
                    DT::dataTableOutput('standings')
                  ),
                  box(
                    width = 12, status = "success", solidHeader = TRUE,
                    title = "Season Results",
                    collapsible = TRUE,
                    #collapsible=TRUE,collapsed = TRUE, bug?
                    DT::dataTableOutput("results")
                    
                  )
                )
                
              )),
      tabItem("matrix",
              
              fluidRow(column(
                width = 10,offset = 1,
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  title = "Match Matrix - Ordered Vertically by Final Position ",
                  plotOutput("plot_matrix")
                )
              ))),
      tabItem("matchup",
              
              fluidRow(
                box(
                  width = 6, status = "success", solidHeader = TRUE,
                  title = "Head To Head - click row for details against opponent",
                  collapsible = TRUE,
                  DT::dataTableOutput("headToHead")
                ),
                box(
                  width = 6, status = "success", solidHeader = TRUE,
                  title = "All Games",
                  #textOutput("check"))
                  DT::dataTableOutput("HtoHGames")
                )
              ),
              fluidRow(
                box(
                  width = 8,
                  status = "success", solidHeader = TRUE,
                  title = "Summary of Seasons Difference in Overall Position",
                  ggvisOutput("HtoHPos")
                )
              )),
      
      tabItem("pfaPlayers",
              fluidRow(
                box(
                  width = 12,
                  div(
                    style = "display:inline-block;padding-right: 20px; border-color: #00a65a;width: 200px;",h5(
                      "Since 1973 the PFA have chosen their best team for each division.
                      Make Selections to analyse the results"
                    )
                    ),
                  div(style = "display:inline-block",selectInput(
                    "pfaPlayer","Player",c("All",playerChoice), width = 180
                  )),
                  div(style = "display:inline-block;",selectInput(
                    "pfaTeam","Team",c("All",teamChoice), width = 180
                  )),
                  div(style = "display:inline-block",selectInput(
                    "pfaCountry","Country",c("All",countryChoice), width = 180
                  )),
                  div(style = "display:inline-block",selectInput(
                    "pfaTier","Tier",c("All",1,2,3,4), width = 80
                  )),
                  div(style = "display:inline-block",selectInput(
                    "pfaPosition","Position",c("All","GK","DF","MF","FW"), width = 80
                  ))
                  )
    ),
    
    
    fluidRow(
      column(
        width = 6,
        box(
          width = 12, status = "success", solidHeader = TRUE,
          title = "Click on Player for Detailed Data",
          DT::dataTableOutput("pfaTable")
          
        )
      ),
      column(
        width = 6,
        box(
          width = 12,status = "success", solidHeader = TRUE,
          title = "Click on Season for Full Team",
          collapsible = TRUE, collapsed = FALSE,
          DT::dataTableOutput("pfaPlayerTable")
        ),
        box(
          width = 12,
          
          collapsible = TRUE, collapsed = FALSE,
          DT::dataTableOutput("pfaSeasonTier")
        )
      )
    )),
    
    
    
    tabItem("info", includeMarkdown("info.md"))
    )#tabitems
      ) #body
  ) #page
