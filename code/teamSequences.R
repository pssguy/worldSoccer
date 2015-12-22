## add current

## sort venue link


dataTeamSeqs <- eventReactive(input$seq_Button,{
  if (input$seq_Opp=="All") {
  home <- df %>% 
    filter(home==input$seq_Team) %>% 
    mutate(gameOrder=row_number(),win=ifelse(result=="H",1,0),
           loss=ifelse(result=="A",1,0),nowin=ifelse(result!="H",1,0),noloss=ifelse(result!="A",1,0),
           team=home)
  
  away <- df %>% 
    filter(visitor==input$seq_Team) %>% 
    mutate(gameOrder=row_number(),win=ifelse(result=="A",1,0),
           loss=ifelse(result=="H",1,0),nowin=ifelse(result!="A",1,0),noloss=ifelse(result!="H",1,0),
           team=visitor)
  
  allGames <- rbind(home,away) %>% 
    arrange(gameDate) %>% 
    mutate(gameOrder=row_number())
  } else {
    home <- df %>% 
      filter(home==input$seq_Team&visitor==input$seq_Opp) %>% 
      mutate(gameOrder=row_number(),win=ifelse(result=="H",1,0),
             loss=ifelse(result=="A",1,0),nowin=ifelse(result!="H",1,0),noloss=ifelse(result!="A",1,0),
             team=home)
    
    away <- df %>% 
      filter(visitor==input$seq_Team&home==input$seq_Opp) %>% 
      mutate(gameOrder=row_number(),win=ifelse(result=="A",1,0),
             loss=ifelse(result=="H",1,0),nowin=ifelse(result!="A",1,0),noloss=ifelse(result!="H",1,0),
             team=visitor)
    
    allGames <- rbind(home,away) %>% 
      arrange(gameDate) %>% 
      mutate(gameOrder=row_number())
  }  
  
  print(glimpse(allGames))
  print(input$seq_Venue)
  print(input$seq_Category)
  
  
  if (input$seq_Venue=="All") {
  if (input$seq_Category=="Win") {
  test <- allGames %>% 
    select(team,gameDate,goaldif,win)
 # print(glimpse(test))
  df_seq <- test %>%
    do (subSeq(.$win)) %>% 
    filter(value==1) %>% 
    mutate(gameOrder=as.integer(first))
 # print(glimpse(df_seq))
  } else if (input$seq_Category=="No Win"){
    df_seq <- allGames %>% 
      select(team,gameDate,goaldif,win) %>%
      do (subSeq(.$win)) %>% 
      filter(value==0) %>% 
      mutate(gameOrder=as.integer(first))
  } else if (input$seq_Category=="Loss"){
    df_seq <- allGames %>% 
      select(team,gameDate,goaldif,loss) %>%
      do (subSeq(.$loss)) %>% 
      filter(value==1) %>% 
      mutate(gameOrder=as.integer(first))
  } else if (input$seq_Category=="No Loss"){
   
    df_seq <- allGames %>% 
      select(team,gameDate,goaldif,loss) %>%
      do (subSeq(.$loss)) %>% 
      filter(value==0) %>% 
      mutate(gameOrder=as.integer(first))
  }
  } else if (input$seq_Venue=="Home") {
    if (input$seq_Category=="Win") {
      df_seq <- home %>% 
        select(team,gameDate,goaldif,win) %>%
        do (subSeq(.$win)) %>% 
        filter(value==1) %>% 
        mutate(gameOrder=as.integer(first))
    } else if (input$seq_Category=="No Win"){
      df_seq <- home %>% 
        select(team,gameDate,goaldif,win) %>%
        do (subSeq(.$win)) %>% 
        filter(value==0) %>% 
        mutate(gameOrder=as.integer(first))
    } else if (input$seq_Category=="Loss"){
      df_seq <- home %>% 
        select(team,gameDate,goaldif,loss) %>%
        do (subSeq(.$loss)) %>% 
        filter(value==1) %>% 
        mutate(gameOrder=as.integer(first))
    } else if (input$seq_Category=="No Loss"){
      
      df_seq <- home %>% 
        select(team,gameDate,goaldif,loss) %>%
        do (subSeq(.$loss)) %>% 
        filter(value==0) %>% 
        mutate(gameOrder=as.integer(first))
    }
  }
  #print(glimpse(df_seq))
  info=list(df_seq=df_seq,allGames=allGames,away=away,home=home)
  return(info)
  
})

output$teamSeqs <- renderPlotly({
#   print("seqValue")
#   print(input$seq_Venue)
#   print("are we good")
#   print(glimpse(dataTeamSeqs()$df_seq))
#   print("not sure")
  if (is.null(dataTeamSeqs()$df_seq)) return() 
  
 # print(glimpse(dataTeamSeqs()$df_seq))
  df_seq <- dataTeamSeqs()$df_seq
  allGames <- dataTeamSeqs()$allGames
  home <- dataTeamSeqs()$home
  away <- dataTeamSeqs()$away
  print(glimpse(dataTeamSeqs()$df_seq))
  
  if (input$seq_Venue=="All") {
chart <-  df_seq %>% 
    filter(slength>=input$seq_Run) %>% 
    left_join(allGames) %>% 
    rename(Sequence=slength)

  } else if (input$seq_Venue=="Home") {
    chart <-     df_seq %>% 
      filter(slength>=input$seq_Run) %>% 
      left_join(home) %>% 
      rename(Sequence=slength) 
  } else if (input$seq_Venue=="Away") {
    chart <-     df_seq %>% 
      filter(slength>=input$seq_Run) %>% 
      left_join(away) %>% 
      rename(Sequence=slength) 
  } 
  plot_ly(chart,x=gameDate,y=Sequence,type="bar",hoverinfo = "text",
          text = paste0("From:",gameDate)) %>%
    layout(hovermode = "closest",
           xaxis=list(title="")
    )
})