##


heatData <- reactive({
  # if (is.null(input$heatTeam))
  #   return()
  # if (input$heatTeam == "")
  #   return()
  req(input$heatTeam)
  req(input$heatYears)
  
  startYr <- input$heatYears[1]
  endYr <- input$heatYears[2]
  
  print(input$heatTeam)
  home <- df %>%
    filter(home == input$heatTeam) %>%
    rename(GF = hgoal,GA = vgoal)
  
  away <- df %>%
    filter(visitor == input$heatTeam) %>%
    rename(GF = vgoal,GA = hgoal)
  
  
  total <- rbind(home,away)
  total$team <- input$heatTeam
  
  # newly added in season limit here
  total <- total %>%
    filter(Season >= startYr & Season <= endYr) %>%
    mutate(
      Opponents = ifelse(home == team,visitor,home),Venue = ifelse(home == team,"Home","Away")
    )
  
  # result summary
  if (input$heatOpponent != "All Teams") {
    W <- total %>%
      filter(team == input$heatTeam & 
               Opponents == input$heatOpponent & GF > GA) %>%
      tally() %>%
      .$n  ##55
    
    D <- total %>%
      filter(team == input$heatTeam &
               Opponents == input$heatOpponent & GF == GA) %>%
      tally() %>%
      .$n
    
    L <- total %>%
      filter(team == input$heatTeam &
               Opponents == input$heatOpponent & GF < GA) %>%
      tally() %>%
      .$n
  } else {
    W <- total %>%
      filter(team == input$heatTeam & GF > GA) %>%
      tally() %>%
      .$n  ##55
    
    D <- total %>%
      filter(team == input$heatTeam & GF == GA) %>%
      tally() %>%
      .$n
    
    L <- total %>%
      filter(team == input$heatTeam & GF < GA) %>%
      tally() %>%
      .$n
  }
  
  
  summary <- paste0("Results Summary ",W,"W-",D,"D-",L,"L")
  
  # print(sort(names(total)))
  
  write_csv(total,"total.csv")
  
  info = list(total = total,summary = summary)
  return(info)
})

output$heatSummary <- renderText({
  # if (is.null(heatData))
  #   return()
  
  heatData()$summary
})

output$heatResultsOrder <- DT::renderDataTable({
  if (is.null(heatData))
    return()
  
  print(glimpse(heatData()$total))
  print("heatResultsOrder info")
  
  if (input$heatOpponent!="All Teams") {
  heatData()$total %>%
    filter(Opponents==input$heatOpponent) %>% 
    mutate(scoreline=paste0(GF,"-",GA)) %>% 
    group_by(scoreline) %>%
    tally() %>%
    arrange(desc(n)) %>%
    rename(count = n) %>%
    DT::datatable(
      rownames = TRUE,options = list(
        paging = TRUE, searching = FALSE,info = FALSE
      )
    )
  } else {
    heatData()$total %>%
      mutate(scoreline=paste0(GF,"-",GA)) %>% 
      group_by(scoreline) %>%
      tally() %>%
      arrange(desc(n)) %>%
      rename(count = n) %>%
      DT::datatable(
        rownames = TRUE,options = list(
          paging = TRUE, searching = FALSE,info = FALSE
        )
      )
  }
  
})

output$heatResults <- renderPlotly({
  # if (is.null(heatData))
  #   return()
  total <- heatData()$total
  
  # write_csv(total,"problem.csv") #1 2016-02-28   2015 Liverpool Everton resch.    NA    NA        1     1      NA      NA     NA 2016-02-28 Liverpool   Everton  Home
  # 
  # print("total")
  # print(total)
  # 
  # ## then do the matrix##
  # 
  # 
  # ## check max
  # 
  # print(total$GF)
  # 
  # temp <- total[is.na(total$GF),]
  # print(temp)
  
  # to cater for actual ocurrence wher futeure games with GF=NA are included
  total <- total %>% 
    filter(!is.na(GF))
  
  print(maxGF <- max(total$GF))
  print(maxGA <- max(total$GA))
  
  
  
  theMax <- max(c(maxGF,maxGA))
  
  print("themax")
  print(theMax) #NA
  
  allCombos <- expand.grid(data.frame(GF = 0:theMax,GA = 0:theMax)) %>%
    mutate(combo = paste0(GF,GA)) #169 13x13
  
  ## this can be filtered by oppo year etc as required
  
  startYr <- input$heatYears[1]
  endYr <- input$heatYears[2]
  
  print(startYr)
  print(endYr)
  print(input$heatOpponent)
  
  if (input$heatOpponent == "All Teams") {
    temp <- total %>%
      filter(Season >= startYr & Season <= endYr) %>%
      mutate(combo = paste0(GF,GA)) %>%
      group_by(combo) %>%
      tally()
  } else {
    temp <- total %>%
      filter(Opponents == input$heatOpponent &
               Season >= startYr & Season <= endYr) %>%
      mutate(combo = paste0(GF,GA)) %>%
      group_by(combo) %>%
      tally()
  }
  
  print(nrow(temp))
  if (nrow(temp) == 0)
    return()
  
  test <- allCombos %>%
    left_join(temp) %>%
    select(GF,GA,count = n)
  
  # need to transform
  Games <-
    t(matrix(
      test$count, nrow = theMax + 1, ncol = theMax + 1, byrow = TRUE,
      dimnames = list(unique(test$GF),
                      unique(test$GA))
    ))
  
  
  plot_ly(
    x = unique(test$GF), y = unique(test$GF), z = Games, key = Games, hoverinfo =
      "z",
    colorscale = 'YIOrRd', reversescale = T,
    type = "heatmap"
  ) %>%
    layout(xaxis = list(title = "Goals Against"),
           yaxis = list(title = "Goals For"))
  
  
  
  
})

# output$heatResults <- renderPlotly({
#
#   if (is.null(input$heatTeam)) return()
#   if (input$heatTeam=="") return()
#
#   print(input$heatTeam)
#   home <- df %>%
#     filter(home==input$heatTeam) %>%
#     rename(GF=hgoal,GA=vgoal)
#
#   away <- df %>%
#     filter(visitor==input$heatTeam) %>%
#     rename(GF=vgoal,GA=hgoal)
#
#
#   total <- rbind(home,away)
#   total$team<-input$heatTeam
#
#  total <- total %>%
#     mutate(Opponents=ifelse(home==team,visitor,home),Venue= ifelse(home==team,"Home","Away"))
#
#   print(sort(names(total)))
#
#   ## then do the matrix##
#
#
#   ## check max
#
#   maxGF<-max(total$GF)
#   maxGA<-max(total$GA)
#
#   theMax <- max(c(maxGF,maxGA))
#
#   allCombos <- expand.grid(
#     data.frame(GF=0:theMax,GA=0:theMax)
#   ) %>%
#     mutate(combo=paste0(GF,GA)) #169 13x13
#
#   ## this can be filtered by oppo year etc as required
#
#   startYr <- input$heatYears[1]
#   endYr <- input$heatYears[2]
#
#   print(startYr)
#   print(endYr)
#   print(input$heatOpponent)
#
#   if(input$heatOpponent=="All Teams") {
#   temp <- total %>%
#     filter(Season>=startYr&Season<=endYr) %>%
#     mutate(combo=paste0(GF,GA)) %>%
#     group_by(combo) %>%
#     tally()
#   } else {
#     temp <- total %>%
#       filter(Opponents==input$heatOpponent&Season>=startYr&Season<=endYr) %>%
#       mutate(combo=paste0(GF,GA)) %>%
#       group_by(combo) %>%
#       tally()
#   }
#
#   print(nrow(temp))
#   if (nrow(temp)==0) return()
#
#   test <- allCombos %>%
#     left_join(temp) %>%
#     select(GF,GA,count=n)
#
#   # need to transform
#   Games <- t(matrix(test$count, nrow = theMax+1, ncol = theMax+1, byrow = TRUE,
#                     dimnames = list(unique(test$GF),
#                                     unique(test$GA))))
#
#
#   plot_ly(x = unique(test$GF), y = unique(test$GF), z = Games, key = Games, hoverinfo="z",
#           colorscale='YIOrRd', reversescale=T,
#           type = "heatmap") %>%
#     layout(xaxis = list(title = "Goals Against"),
#            yaxis = list(title = "Goals For"))
#
#
#
#
# })

## crosstalk to get to table of those results
#cv <- crosstalk::ClientValue$new("plotly_click", group = "A")


output$heatHeader <- renderUI({
  #s <- cv$get()
  s <- event_data("plotly_click")
  if (length(s) == 0)
    return()
  
  gFor = s[["y"]]
  gAg = s[["x"]]
  
  
  if (gFor > gAg) {
    h4(paste0(gFor,"-",gAg, " victories"))
  } else if (gAg > gFor) {
    h4(paste0(gFor,"-",gAg, " losses"))
  } else {
    h4(paste0(gFor,"-",gAg, " draws"))
  }
})

output$heatTable <- DT::renderDataTable({
 # s <- cv$get()
  s <- event_data("plotly_click")
  print(("printing s"))
  print(length(s))
  if (length(s) == 0) {
    return()
  } else {
    gFor = as.integer(s[["y"]])
    gAg = as.integer(s[["x"]])
    print(input$heatTeam)
    print(gFor)
    print(gAg)
    
    home <- df %>%
      filter(home == input$heatTeam) %>%
      rename(GF = hgoal,GA = vgoal)
    
    away <- df %>%
      filter(visitor == input$heatTeam) %>%
      rename(GF = vgoal,GA = hgoal)
    
    
    total <- rbind(home,away)
    total$team <- input$heatTeam
    
    print(sort(names(total)))
    
    print(glimpse(total))
    
    startYr <- input$heatYears[1]
    endYr <- input$heatYears[2]
    
    if (input$heatOpponent == "All Teams") {
      prob <-   total %>%
        filter(GF == gFor &
                 GA == gAg & team == input$heatTeam &
                 Season >= startYr & Season <= endYr) %>%
        mutate(
          Opponents = ifelse(home == team,visitor,home),Venue = ifelse(home == team,"Home","Away")
        ) %>%
        arrange(desc(gameDate))
    } else {
      prob <-   total %>%
        filter(GF == gFor & GA == gAg & team == input$heatTeam) %>%
        mutate(
          Opponents = ifelse(home == team,visitor,home),Venue = ifelse(home == team,"Home","Away")
        ) %>%
        filter(Opponents == input$heatOpponent &
                 Season >= startYr & Season <= endYr) %>%
        arrange(desc(gameDate))
      
      
    }
    
    
    print(glimpse(prob)) ## comes out with previous club eg when switching form crystal palace to crawlwy
    
    prob %>%
      select(Opponents,Venue,Season,Tier = tier,
             Date = gameDate) %>%
      DT::datatable(
        class = 'compact stripe hover row-border order-column',rownames = FALSE,options = list(
          paging = TRUE, searching = TRUE,info = FALSE
        )
      )
    
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
