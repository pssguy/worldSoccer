# head to head 

headData <- reactive({
  #if(is.null(input$team)) return()
  
  req(input$team)
  
  home <- df %>% 
    filter(home==input$team) %>% 
    group_by(visitor) %>% 
    summarise(P = n(), GF = sum(hgoal), GA = sum(vgoal), GD=sum(goaldif),
              W=sum(result=="H"), D=sum(result=="D"), L=sum(result=="A") ) %>% 
    rename(opponent=visitor)
  away <- df %>% 
    filter(visitor==input$team) %>% 
    group_by(home) %>% 
    summarise(P = n(), 
              W=sum(result=="A"), D=sum(result=="D"), L=sum(result=="H"),GF = sum(vgoal), GA = sum(hgoal), GD=GF-GA )%>% 
    rename(opponent=home)
  
  
  
  total <- rbind(home,away) 
  
  summary <- total %>% 
    group_by(opponent) %>% 
    summarize(P=sum(P),W=sum(W),D=sum(D),L=sum(L),GF=sum(GF),GA=sum(GA),GD=GF-GA) 

  
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

  s = as.integer(input$headToHead_rows_selected)
  
  theOpponent <-headData()$summary$opponent[s]
  
  df %>% 
    filter((home==input$team&visitor==theOpponent)|(home==theOpponent&visitor==input$team)) %>% 
    select(Date,home,FT,visitor,tier) %>% 
    arrange(desc(Date)) %>% 
    DT::datatable(rownames=TRUE,selection='single',options= list(pageLength=10,
                                                                 paging = TRUE, searching = FALSE,info=FALSE))
  
  
})


observe({
  if(is.null(input$headToHead_rows_selected)) return()
 
 
  s = as.integer(input$headToHead_rows_selected)
  
  teamB <-headData()$summary$opponent[s]
  teamA <- input$team
  
  test <-all %>% 
    select(Season,team,Overall) %>% 
    filter(team==teamA|team==teamB) %>% 
    spread(team,Overall) 
  
  
  if(teamA<teamB) {
    
    colnames(test) <- c("Season","team","opponent")
    
  } else {
    colnames(test) <- c("Season","opponent","team")  
  }
  
  test %>% 
    mutate(diff=opponent-team) %>% 
    filter(!is.na(diff)) %>% 
    ggvis(~diff,fill:='#f39c12') %>% 
    add_axis("x", title="Difference in Overall Standing", format='d') %>% 
    add_axis("y", title= "Seasons", format='d') %>% 
    bind_shiny("HtoHPos")
  
})

