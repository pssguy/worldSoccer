

output$standingsPlot <- renderPlotly({
  
  req(input$team)
  selection<-  all %>% 
    filter(team==input$team) 
  
  theTitle=paste0("League position by Year - ",input$team)
  
  ## may need to set colors in data.frame 
  p <-plot_ly(selection, x = Season, y = Overall, mode = "markers",
              color=tier,
              marker=list(size=6), hoverinfo="text", 
              
              text = paste0("Season: ",Season,
                            
                            "<br>Tier: ",tier,
                            "<br>Position: ",Position,
                            "<br>Overall: ",Overall
              )) %>%
    layout(hovermode = "closest",
           showlegend = F,
            title=theTitle,
           xaxis=list(title=""),
          
           yaxis=list(title="Overall League Position", autorange="reversed", zeroline =FALSE
           )
    ) %>% 
    config(displayModeBar = F,showLink = F)
  p
})

standingsData <- reactive({
  req(input$team)
  s <- event_data("plotly_click")
  
  if (length(s) == 0) {
    return()
  } else {
    theSeason = as.integer(s[["x"]]) 
    
    # get tier so print correct table
    theDivision <-  all %>% 
      filter(Season==theSeason&team==input$team) %>% 
      .$division
  } 
    info=list(theSeason=theSeason,theDivision=theDivision)
    return(info)
  
})

output$standingsTable <- DT::renderDataTable({
 if(is.null(standingsData())) return()
    
    all %>% 
      filter(Season==standingsData()$theSeason&division==standingsData()$theDivision) %>% 
      arrange(Position) %>% 
      
      select(team,Pl=GP,W,D,L,GD=gd,Pts) %>% 
      
      DT::datatable(options= list(scrollY = 500,paging = FALSE, searching = FALSE,info=FALSE))
  
})

output$standingsTableTitle <- renderText({
  if(is.null(standingsData())) return()
  
  paste0("League Table - ",standingsData()$theSeason," Tier ",standingsData()$theDivision)
  
})

output$resultsTitle <- renderText({
  if(is.null(standingsData())) return()
  
  paste0("Season Results - ",input$team," ",standingsData()$theSeason)
  
})

output$results <- DT::renderDataTable({
  if(is.null(standingsData())) return()
 
  
  df %>% 
    filter(Season==standingsData()$theSeason&(home==input$team|visitor==input$team)) %>% 
    mutate(result=paste(hgoal,vgoal,sep=" - ")) %>% 
    select(Date,home,result,visitor) %>% 
    arrange(Date) %>% 
    DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
})