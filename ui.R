

dashboardPage(
  skin = "yellow",
  
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
        menuSubItem("Season Cumulatives", tabName = "cumulative", selected =T),
        menuSubItem("Standings", tabName = "standings")
      #  menuSubItem("Sequences", tabName = "sequences")
      ),
     
      menuItem(
        "Other Leagues",
        menuSubItem("Bundesliga",href = "https://mytinyshinys.shinyapps.io/bundesliga"),
        menuSubItem("Eredivise",href = "https://mytinyshinys.shinyapps.io/eredivise"),
        menuSubItem("La Liga",href = "https://mytinyshinys.shinyapps.io/laLiga"),
        menuSubItem("Serie A",href = "https://mytinyshinys.shinyapps.io/seriea")
      ),
      menuItem("PFA Awards", tabName = "pfaPlayers"),
      menuItem("Deloitte Rankings", tabName = "deloitte"),
      
      tags$hr(),
      menuItem(text="",href="https://mytinyshinys.shinyapps.io/dashboard",badgeLabel = "All Dashboards and Trelliscopes (14)"),
      tags$hr(),
      
      tags$body(
        a(class="addpad",href="https://twitter.com/pssGuy", target="_blank",img(src="images/twitterImage25pc.jpg")),
        a(class="addpad2",href="mailto:agcur@rogers.com", img(src="images/email25pc.jpg")),
        a(class="addpad2",href="https://github.com/pssguy",target="_blank",img(src="images/GitHub-Mark30px.png")),
        a(href="https://rpubs.com/pssguy",target="_blank",img(src="images/RPubs25px.png"))
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
#       tabItem("sequences",
#               
#               fluidRow(column(
#                 width = 12,
#                 box(
#                   width = 12,height=800,
#                   status = "success", solidHeader = TRUE,
#                   title = "Sequences",
#                  dimpleOutput("teamSeqs")
#                 )
 #            ))),
      
      tabItem("cumulative",
              
              fluidRow(column(
                width = 6,
                box(
                  width = 12,
                  status = "success", solidHeader = TRUE,
                  title = "Cumulative", footer="Current Season and Premier League years highlighted",
                  plotOutput("cumulativePlot")
                )
              ))),
      
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
    
    
    tabItem("deloitte",
           
            
            box(
              radioButtons("delCount",label=NULL,choices=c("Top 10","Top 20","Top 30"), inline=T),
              width = 12,status = "success", solidHeader = TRUE,
              title = "Number of Top Ranked Clubs by Country",
              collapsible = TRUE, collapsed = TRUE,
              fluidRow(
                column(width=7,ggvisOutput("delByCountry")),
                column(width=3,offset=1,DT::dataTableOutput("delTeamCount"))
              )),
              
              box(
                radioButtons("delCategory",label=NULL,choices=c("Rank","Revenue","Index (2011=100)"), inline=T),
                width = 8,offset=2,status = "success", solidHeader = TRUE,
                title = "Top Ten Clubs - 2013/14 ",
                collapsible = TRUE, collapsed = FALSE,
                ggvisOutput("delTopTen")
            
            ),
            box(
              width = 8,offset=2,status = "success", solidHeader = TRUE,
              title = "Data Summary",
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("deloitteData"))
            ),
    
    
    
    tabItem("info", includeMarkdown("info.md"))
    )#tabitems
      ) #body
  ) #page
