



dashboardPage(
  skin = "yellow",
  title = "English Leagues",
  
  dashboardHeader(title = "English Leagues"),
  dashboardSidebar(
    includeCSS("custom.css"),
    
    uiOutput("a"),
    
    uiOutput("b"),
    
    sidebarMenu(
      id = "sbMenu",
      
      
      menuItem(
        "Team History",
        menuSubItem("Head To Head", tabName = "matchup"),
        menuSubItem("Result Matrix", tabName = "matrix"),
        menuSubItem("Season Cumulatives", tabName = "cumulative"),
        menuSubItem("Scoreline Heatmap", tabName = "tm_heat"),
        menuSubItem("Standings", tabName = "standings", selected = T),
        menuSubItem("Sequences", tabName = "sequences")
      ),
      
      menuItem(
        "Other Leagues",
        menuSubItem("Bundesliga", href = "https://mytinyshinys.shinyapps.io/bundesliga"),
        menuSubItem("Eredivise", href = "https://mytinyshinys.shinyapps.io/eredivise"),
        menuSubItem("La Liga", href = "https://mytinyshinys.shinyapps.io/laLiga"),
        menuSubItem("Serie A", href = "https://mytinyshinys.shinyapps.io/seriea")
      ),
      menuItem("PFA Awards", tabName = "pfaPlayers"),
      menuItem("Deloitte Rankings", tabName = "deloitte"),
      
      tags$hr(),
      menuItem(
        text = "",
        href = "https://mytinyshinys.shinyapps.io/dashboard",
        badgeLabel = "All Dashboards and Trelliscopes (14)"
      ),
      tags$hr(),
      
      tags$body(
        a(
          class = "addpad",
          href = "https://twitter.com/pssGuy",
          target = "_blank",
          img(src = "images/twitterImage25pc.jpg")
        ),
        a(
          class = "addpad2",
          href = "mailto:agcur@rogers.com",
          img(src = "images/email25pc.jpg")
        ),
        a(
          class = "addpad2",
          href = "https://github.com/pssguy",
          target = "_blank",
          img(src = "images/GitHub-Mark30px.png")
        ),
        a(
          href = "https://rpubs.com/pssguy",
          target = "_blank",
          img(src = "images/RPubs25px.png")
        )
      )
      
      
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
                  width = 5,
                  offset = 1,
                  box(
                    width = 12,
                    status = "success",
                    solidHeader = TRUE,
                   
                    footer = "Zoomable. Hover for Info. Click for League Table",
                    plotlyOutput("standingsPlot")
                  )
                ),
                column(
                  width = 5,
                  offset = 1,
                  box(
                    width = 12,
                    status = "success",
                    solidHeader = TRUE,
                    
                    h4(textOutput("standingsTableTitle")),
                    DT::dataTableOutput('standingsTable')
                  )
                )
              ),
              
              fluidRow(column(
                width = 5,
                offset = 1,
                box(
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  
                  h4(textOutput("resultsTitle")),
                  DT::dataTableOutput("results")
                  
                )
              ))),
      
      tabItem("matrix",
              
              fluidRow(column(
                width = 10,
                offset = 1,
                box(
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  title = "Match Matrix - Ordered Vertically by Final Position ",
                  plotOutput("plot_matrix", height = "600px")
                )
              ))),
      tabItem("matchup",
              
              fluidRow(
                box(
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  title = "Head To Head - click row for details against opponent",
                  collapsible = TRUE,
                  DT::dataTableOutput("headToHead")
                ),
                box(
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  title = "All Games",
                  #textOutput("check"))
                  DT::dataTableOutput("HtoHGames")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  title = "Summary of Seasons Difference in Overall Position",
                  ggvisOutput("HtoHPos")
                )
              )),
      tabItem("sequences",
              
              fluidRow(column(
                width = 12,
                box(
                  width = 12,
                  height = 500,
                  status = "success",
                  solidHeader = TRUE,
                  title = "Sequences",
                  footer = "With Specific Opposition, Gaps may indicate no games played",
                  plotlyOutput("teamSeqs")
                )
              ))),
      
      tabItem(
        "cumulative",
        
        
        box(
          width = 6,
          status = "success",
          
          footer = "Current Season and Premier League years highlighted",
          plotOutput("cumulativePlot")
        ),
        box(
          width = 6,
          status = "success",
          
          footer = "Points are jittered to allow hover info. Still in beta",
          
          plotlyOutput("cumulativeGameOrderPlot")
        )
      ),
      
      
      tabItem("pfaPlayers",
              fluidRow(
                box(
                  width = 12,
                  div(
                    style = "display:inline-block;padding-right: 20px; border-color: #00a65a;width: 200px;",
                    h5(
                      "Since 1973 the PFA have chosen their best team for each division.
                      Make Selections to analyse the results"
                    )
                    ),
                  div(style = "display:inline-block", selectInput(
                    "pfaPlayer", "Player", c("All", playerChoice), width = 180
                  )),
                  div(style = "display:inline-block;", selectInput(
                    "pfaTeam", "Team", c("All", teamChoice), width = 180
                  )),
                  div(style = "display:inline-block", selectInput(
                    "pfaCountry", "Country", c("All", countryChoice), width = 180
                  )),
                  div(style = "display:inline-block", selectInput(
                    "pfaTier", "Tier", c("All", 1, 2, 3, 4), width = 80
                  )),
                  div(style = "display:inline-block", selectInput(
                    "pfaPosition",
                    "Position",
                    c("All", "GK", "DF", "MF", "FW"),
                    width = 80
                  ))
                  )
    ),
    
    
    fluidRow(
      column(
        width = 6,
        box(
          width = 12,
          status = "success",
          solidHeader = TRUE,
          title = "Click on Player for Detailed Data",
          DT::dataTableOutput("pfaTable")
          
        )
      ),
      column(
        width = 6,
        box(
          width = 12,
          status = "success",
          solidHeader = TRUE,
          title = "Click on Season for Full Team",
          collapsible = TRUE,
          collapsed = FALSE,
          DT::dataTableOutput("pfaPlayerTable")
        ),
        box(
          width = 12,
          
          collapsible = TRUE,
          collapsed = FALSE,
          DT::dataTableOutput("pfaSeasonTier")
        )
      )
    )),
    
    
    tabItem(
      "deloitte",
      
      
      box(
        radioButtons(
          "delCount",
          label = NULL,
          choices = c("Top 10", "Top 20", "Top 30"),
          inline = T
        ),
        width = 12,
        status = "success",
        solidHeader = TRUE,
        title = "Number of Top Ranked Clubs by Country",
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(width = 7, ggvisOutput("delByCountry")),
          column(
            width = 3,
            offset = 1,
            DT::dataTableOutput("delTeamCount")
          )
        )
      ),
      
      box(
        radioButtons(
          "delCategory",
          label = NULL,
          choices = c("Rank", "Revenue", "Index (2011=100)"),
          inline = T
        ),
        width = 8,
        offset = 2,
        status = "success",
        solidHeader = TRUE,
        title = "Top Ten Clubs - 2013/14 ",
        collapsible = TRUE,
        collapsed = FALSE,
        ggvisOutput("delTopTen")
        
      ),
      box(
        width = 8,
        offset = 2,
        status = "success",
        solidHeader = TRUE,
        title = "Data Summary",
        collapsible = TRUE,
        collapsed = TRUE,
        DT::dataTableOutput("deloitteData")
      )
    ),
    tabItem("tm_heat",
            
            fluidRow(
              column(
                width = 6,
                box(
                  width = 12,
                  title = "Heatmap",
                  solidHeader = TRUE,
                  status = 'success',
                  
                  textOutput("heatSummary"),
                  plotlyOutput("heatResults"),
                  h4(
                    "Click on a cell in the heatmap to display table of results.   If no chart appears, the teams did not meet during time period"
                  )
                ),
                box(
                  width = 12,
                  title = "Scorelines Summary",
                  solidHeader = TRUE,
                  status = 'success',
                  DT::dataTableOutput("heatResultsOrder")
                )
                
                
              ),
              
              column(
                width = 6,
                box(
                  width = 12,
                  title = "Results by Scoreline",
                  solidHeader = TRUE,
                  status = 'success',
                  
                  uiOutput("heatHeader"),
                  DT::dataTableOutput("heatTable")
                  
                )
              )
            )),
    
    
    
    tabItem("info", includeMarkdown("info.md"))
    )#tabitems
      ) #body
  ) #page
