
pfaData <- reactive({
  
  if (input$pfaTier=="All") {
    tierOne <- 1
    tierTwo <- 4
  } else {
    tierOne <- as.integer(input$pfaTier)
    tierTwo <- as.integer(input$pfaTier)
  }
  print(tierOne)
  print(tierTwo)
  
  if (input$pfaPosition=="All") {
    thePos <- c("GK","DF","MF","FW")
  } else {
    thePos <- input$pfaPosition
  }
  print(thePos)
  
  print(input$pfaYear[1])
  print(input$pfaYear[2])
  
  if (input$pfaTeam=="All") {
    theTeam <- teamChoice
  } else {
    theTeam <- input$pfaTeam
  }
  
  if (input$pfaCountry=="All") {
    theCountry <- countryChoice
  } else {
    theCountry <- input$pfaCountry
  }
  
  if (input$pfaPlayer=="All") {
    thePlayer <- playerChoice
  } else {
    thePlayer <- input$pfaPlayer
  }
  
  res <-   pfa %>% 
    filter(season>=input$pfaYear[1]&season<=input$pfaYear[2]
           &tier>=tierOne&tier<=tierTwo
           &pos %in% thePos
           &team %in% theTeam
           &country %in% theCountry
           &player %in% thePlayer) %>% 
    group_by(player) %>% 
    tally() %>% 
    rename(Years=n) %>% 
    ungroup() %>% 
    arrange(desc(Years))
  
  info=list(res=res)
  return(info)
  
})

output$pfaTable <- DT::renderDataTable({
  
  
  pfaData()$res %>% 
    DT::datatable(rownames=TRUE,selection='single')
  
})

playerData <- reactive({ 
  
  print(input$pfaTable_rows_selected)
  if(is.null(input$pfaTable_rows_selected)) return()
  
  s = as.integer(input$pfaTable_rows_selected)
  
  print(s)
  
  thePlayer <- pfaData()$res[s,]$player
  print(thePlayer)
  
  res <-   pfa %>% 
    filter(player==thePlayer) %>% 
    select(season,team,tier)  
  
  info=list(res=res)
  return(info)
  
  
})



output$pfaPlayerTable <- DT::renderDataTable({ 
  print("enter pfaTable")
  print(pfaData())
  if(is.null(pfaData())) return()
  
  playerData()$res %>% 
    DT::datatable(rownames=TRUE,selection='single',options= list(paging = FALSE, searching = FALSE,info=FALSE,
                                                                 columnDefs = list(list(targets = c(0), visible = FALSE))))
  #DT::datatable(rownames=TRUE,selection='single',options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
  
})


output$pfaSeasonTier <- DT::renderDataTable({ 
  
  print(input$pfaPlayerTable_rows_selected)
  if(is.null(input$pfaPlayerTable_rows_selected)) return()
  
  s = as.integer(input$pfaPlayerTable_rows_selected)
  print("printin pfaPlayerTable s")
  print(s)
  
  theSeason <- playerData()$res[s,]$season
  theTier <- playerData()$res[s,]$tier
  
  pfa %>% 
    filter(season==theSeason&tier==theTier) %>% 
    select(player,pos,team)   %>% 
    arrange(team) %>% 
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
  
  
  
})


