# head to head 

headData <- reactive({
  print(input$team)
  
  home <- df %>% 
    filter(home==input$team) %>% 
    group_by(visitor) %>% 
    summarise(P = n(), GF = sum(hgoal), GA = sum(vgoal), GD=sum(goaldif),
              W=sum(result=="H"), D=sum(result=="D"), L=sum(result=="A") ) %>% 
    rename(opponent=visitor)
  away <- df %>% 
    filter(visitor==input$team) %>% 
    group_by(home) %>% 
    summarise(P = n(), GF = sum(vgoal), GA = sum(hgoal), GD=GF-GA,
              W=sum(result=="A"), D=sum(result=="D"), L=sum(result=="H") )%>% 
    rename(opponent=home)
  
  
  
  total <- rbind(home,away) 
  
  summary <- total %>% 
    group_by(opponent) %>% 
    summarize(P=sum(P),GF=sum(GF),GA=sum(GA),GD=GF-GA,W=sum(W),D=sum(D),L=sum(L)) 
  
  #     print(row.names(total))
  #     row.names(total) <- total$opponent
  #     print(row.names(total))
  
  info=list(total=total,summary=summary)
  return(info)
  
})



output$headToHead <- DT::renderDataTable({
  headData()$summary %>%  
    DT::datatable(rownames=TRUE,selection='single',options= list(pageLength=10,
                                                                 paging = TRUE, searching = TRUE,info=FALSE))
})


output$HtoHGames <- DT::renderDataTable({
  
  if(is.null(input$headToHead_rows_selected)) return()
  s = input$headToHead_rows_selected
  
  theOpponent <-headData()$summary$opponent[s]
  
  df %>% 
    filter((home==input$team&visitor==theOpponent)|(home==theOpponent&visitor==input$team)) %>% 
    select(Date,home,FT,visitor) %>% 
    arrange(desc(Date)) %>% 
    DT::datatable(rownames=TRUE,selection='single',options= list(pageLength=10,
                                                                 paging = TRUE, searching = FALSE,info=FALSE))
  
  
  
  
})