


data <- eventReactive(input$seq_Button,{
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
  
  all <- rbind(home,away) %>% 
    arrange(gameDate) %>% 
    mutate(gameOrder=row_number())
  
  if (input$seq_category=="Win") {
  df_seq <- all %>% 
    select(team,gameDate,goaldif,win) %>%
    do (subSeq(.$win)) %>% 
    filter(value==1) %>% 
    mutate(gameOrder=as.integer(first))
  } else if (input$seq_category=="No Win"){
    df_seq <- all %>% 
      select(team,gameDate,goaldif,win) %>%
      do (subSeq(.$win)) %>% 
      filter(value==0) %>% 
      mutate(gameOrder=as.integer(first))
  } else if (input$seq_category=="Loss"){
    df_seq <- all %>% 
      select(team,gameDate,goaldif,loss) %>%
      do (subSeq(.$loss)) %>% 
      filter(value==1) %>% 
      mutate(gameOrder=as.integer(first))
  } else if (input$seq_category=="No Loss"){
   
    df_seq <- all %>% 
      select(team,gameDate,goaldif,loss) %>%
      do (subSeq(.$loss)) %>% 
      filter(value==0) %>% 
      mutate(gameOrder=as.integer(first))
  }
  
  info=list(df_seq=df_seq,all=all)
  return(info)
  
})

output$teamSeqs <- renderDimple({
  
  if (is.null( data()$df_seq)) return()
  
  data()$df_seq %>% 
    filter(slength>=input$seq_Run) %>% 
    left_join(data()$all) %>% 
    rename(Sequence=slength) %>%
    dimple(x="gameDate",y="Sequence",type="bar")#
  
})