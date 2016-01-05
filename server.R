# create a holder for all reactive values
values <- reactiveValues()


function(input, output, session) {
  
  
  ## set up input menu in sidebar

  output$a <- renderUI({
   
    if (input$sbMenu=="matrix") {
     inputPanel(
      sliderInput("yearMatrix", "Choose Season (start year)", min=1870,max=2015,value=2015,sep="")
     )
    } else if (input$sbMenu=="sequences") {
      inputPanel(
        selectInput("seq_Team","Choose Team",teamOptions,selected=values$TEAMNAME),
        uiOutput("seqOpps"),
        numericInput("seq_Run", "Choose Minimum Sequence", min=1,max=100,value=5),
        radioButtons("seq_Venue",label=NULL,c("All","Home","Away"),inline=TRUE),
        radioButtons("seq_Category",label=NULL,c("Win","No Win","Loss","No Loss"),inline=TRUE),
        actionButton("seq_Button","Get Chart")
      )
      
    } else if (input$sbMenu=="cumulative") {
      
      inputPanel(
        
        selectInput("cumteam","Choose Team",teamOptions,selected=values$TEAMNAME),
        radioButtons("cumulative",label="Goals in Game",c("For","Ag","Diff","Total Both Teams"), inline=T)
      )
      
    
      
    } else if (input$sbMenu=="pfaPlayers") {
     print("pfa")
      inputPanel(sliderInput(
        "pfaYear","Year(s)",min = 1973,max = 2015,value = c(1992,2015),sep = "", width =
          200)
      )
      
    } else if (input$sbMenu=="deloitte") {
      helpText("The ", a('Deloitte Football Money League', href='http://www2.deloitte.com/content/dam/Deloitte/uk/Documents/sports-business-group/deloitte-football-money-league-2015.PDF')
                ,"is a ranking of football clubs by revenue
               generated from football operations. It is produced annually by the accountancy
               firm Deloitte and released in early February of each year,
               describing the season most recently finished")
    } else if (input$sbMenu=="tm_heat") {
      inputPanel(
                #selectInput("heatTeam",NULL,c("Choose Team" = "",teamOptions)),
        selectInput("heatTeam",NULL,teamOptions,selected=values$TEAMNAME),
                selectInput("heatOpponent",NULL,c("Choose Opponent" = "",c("All Teams",teamOptions))),
                sliderInput("heatYears","Season Range",min=1888,max=2015,value=c(1992,2015),sep="")
      )
    } else {
      print("normal")
      inputPanel(selectInput("team","Choose Team",teamOptions,selected=values$TEAMNAME))
    }
})
  
  
  
    output$b <- renderUI({
      
      if (input$sbMenu=="matrix") {
        
        if(is.null(input$yearMatrix)) return()
       divChoice <- seasonDiv %>% 
         filter(Season %in% input$yearMatrix)
       
       
       divOpts <- divChoice$division
       inputPanel(
       selectInput("division","Choose Tier",divOpts)
       )
      } else {
        return()
      }
    })
    

    output$seqOpps <- renderUI({
      opponents <- df %>% 
        filter(home==input$seq_Team) %>% 
        select(visitor) %>% 
        unique() %>% 
        arrange(visitor) %>% 
        .$visitor 
      
      selectInput("seq_Opp","Opposition",choices=c("All",opponents))
    })
  

    
    observeEvent(input$team,{
      
      values$TEAMNAME <- input$team
      
    })
    
    observeEvent(input$seq_Team,{
      
      values$TEAMNAME <- input$seq_Team
      
    })
    
    observeEvent(input$heatTeam,{
      
      values$TEAMNAME <- input$heatTeam
      
    })
    
    observeEvent(input$cumteam,{
      
      values$TEAMNAME <- input$cumteam
      
    })
  
  
  
## link to detailed code by menuItem

  
  source("code/standings.R", local=TRUE)  
  source("code/matrix.R", local=TRUE)
  source("code/headToHead.R", local=TRUE)
  source("code/pfaPlayers.R", local=TRUE)
    source("code/deloitte.R", local=TRUE)
    source("code/cumulativesBySeason.R", local=TRUE)
    source("code/teamHeatMap.R", local=TRUE)
    source("code/teamSequences.R", local=TRUE)
    
    

}
