output$heatResults <- renderPlotly({
  
  if (is.null(input$heatTeam)) return()
  if (input$heatTeam=="") return()
  
  print(input$heatTeam)
  home <- df %>% 
    filter(home==input$heatTeam) %>% 
    rename(GF=hgoal,GA=vgoal)
  
  away <- df %>% 
    filter(visitor==input$heatTeam) %>% 
    rename(GF=vgoal,GA=hgoal)
  
  
  total <- rbind(home,away) 
  total$team<-input$heatTeam
  
 total <- total %>% 
    mutate(Opponents=ifelse(home==team,visitor,home),Venue= ifelse(home==team,"Home","Away"))
  
  print(sort(names(total)))
  
  ## then do the matrix##
  
  
  ## check max
  
  maxGF<-max(total$GF)
  maxGA<-max(total$GA)
  
  theMax <- max(c(maxGF,maxGA))
  
  allCombos <- expand.grid(
    data.frame(GF=0:theMax,GA=0:theMax)
  ) %>%
    mutate(combo=paste0(GF,GA)) #169 13x13
  
  ## this can be filtered by oppo year etc as required
  
  startYr <- input$heatYears[1]
  endYr <- input$heatYears[2]
  
  print(startYr)
  print(endYr)
  print(input$heatOpponent)
  
  if(input$heatOpponent=="All Teams") {
  temp <- total %>%
    filter(Season>=startYr&Season<=endYr) %>% 
    mutate(combo=paste0(GF,GA)) %>%
    group_by(combo) %>%
    tally()
  } else {
    temp <- total %>%
      filter(Opponents==input$heatOpponent&Season>=startYr&Season<=endYr) %>% 
      mutate(combo=paste0(GF,GA)) %>%
      group_by(combo) %>%
      tally()
  }
  
  print(nrow(temp))
  if (nrow(temp)==0) return()
  
  test <- allCombos %>%
    left_join(temp) %>% 
    select(GF,GA,count=n)
  
  # need to transform
  Games <- t(matrix(test$count, nrow = theMax+1, ncol = theMax+1, byrow = TRUE,
                    dimnames = list(unique(test$GF),
                                    unique(test$GA))))
  
  
  plot_ly(x = unique(test$GF), y = unique(test$GF), z = Games, key = Games, hoverinfo="z",
          colorscale='YIOrRd', reversescale=T,
          type = "heatmap") %>%
    layout(xaxis = list(title = "Goals Against"), 
           yaxis = list(title = "Goals For"))
  
  
  
  
})

## crosstalk to get to table of those results
cv <- crosstalk::ClientValue$new("plotly_click", group = "A")


output$heatHeader <- renderUI({
  s <- cv$get()
  if (length(s)==0) return()
  
  gFor=s[["y"]]
  gAg =s[["x"]]
  
  
  if (gFor>gAg) {
    h4(paste0(gFor,"-",gAg, " victories"))
  } else if (gAg>gFor) {
    h4(paste0(gFor,"-",gAg, " losses")) 
  } else {
    h4(paste0(gFor,"-",gAg, " draws"))
  }
})

output$heatTable <- DT::renderDataTable({
  
  s <- cv$get() 
  
  if (length(s)==0) {
    return()
  } else {
    
    gFor=as.integer(s[["y"]])
    gAg =as.integer(s[["x"]])
    print(input$heatTeam)
    print(gFor)
    print(gAg)
    
    home <- df %>% 
      filter(home==input$heatTeam) %>% 
      rename(GF=hgoal,GA=vgoal)
    
    away <- df %>% 
      filter(visitor==input$heatTeam) %>% 
      rename(GF=vgoal,GA=hgoal)
    
    
    total <- rbind(home,away) 
    total$team<-input$heatTeam
    
    print(sort(names(total)))
    
    print(glimpse(total))
    
    startYr <- input$heatYears[1]
    endYr <- input$heatYears[2]
    
    if(input$heatOpponent=="All Teams") { 
    prob <-   total %>% 
   filter(GF==gFor&GA==gAg&team==input$heatTeam&Season>=startYr&Season<=endYr) %>% 
      mutate(Opponents=ifelse(home==team,visitor,home),Venue= ifelse(home==team,"Home","Away")) %>% 
            arrange(desc(gameDate))
    } else {
      prob <-   total %>% 
        filter(GF==gFor&GA==gAg&team==input$heatTeam) %>% 
        mutate(Opponents=ifelse(home==team,visitor,home),Venue= ifelse(home==team,"Home","Away")) %>% 
        filter(Opponents==input$heatOpponent&Season>=startYr&Season<=endYr) %>% 
        arrange(desc(gameDate))  
      
      
    }
    
    
 print(glimpse(prob)) ## comes out with previous club eg when switching form crystal palace to crawlwy
 
 prob %>% 
      select(Opponents,Venue,Season,Tier=tier,
             Date=gameDate) %>% 
      DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = TRUE,info=FALSE))
 
#  total %>% 
#    filter(GF==0&GA==9) %>%
#    mutate(Opponents=ifelse(home=="Crystal Palace",visitor,home),Venue= ifelse(home=="Crystal Palace","Home","Away")) %>% 
#    
#    arrange(desc(gameDate)) %>% 
#    select(Opponents,Venue,Season,Tier=tier,
#           Date=gameDate) %>% 
#    DT::datatable(class='compact stripe hover row-border order-column',rownames=FALSE,options= list(paging = TRUE, searching = FALSE,info=FALSE))
  }
  
})
