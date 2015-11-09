## add current

## sort venue link


data <- eventReactive(input$seq_Button,{
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
  
  if (input$seq_Venue=="All") {
  if (input$seq_Category=="Win") {
  df_seq <- allGames %>% 
    select(team,gameDate,goaldif,win) %>%
    do (subSeq(.$win)) %>% 
    filter(value==1) %>% 
    mutate(gameOrder=as.integer(first))
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
  
  info=list(df_seq=df_seq,allGames=allGames,away=away,home=home)
  return(info)
  
})

output$teamSeqs <- renderDimple({
  
  if (is.null( data()$df_seq)) return() 
  
  #print(glimpse( data()$df_seq))
  
  if (input$seq_Venue=="All") {
  data()$df_seq %>% 
    filter(slength>=input$seq_Run) %>% 
    left_join(data()$allGames) %>% 
    rename(Sequence=slength) %>%
    dimple(x="gameDate",y="Sequence",type="bar") %>% 
      xAxis(title="Initial Game Date")  
  } else if (input$seq_Venue=="Home") {
    data()$df_seq %>% 
      filter(slength>=input$seq_Run) %>% 
      left_join(data()$home) %>% 
      rename(Sequence=slength) %>%
      dimple(x="gameDate",y="Sequence",type="bar")#
  } else if (input$seq_Venue=="Away") {
    data()$df_seq %>% 
      filter(slength>=input$seq_Run) %>% 
      left_join(data()$away) %>% 
      rename(Sequence=slength) %>%
      dimple(x="gameDate",y="Sequence",type="bar")#
  } 
  
})