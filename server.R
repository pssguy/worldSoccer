


function(input, output, session) {
  
  getSeason = function(data,location,session){
    
    if(is.null(data)) return(NULL)
    
    theSeason <- data$Season
#     updateDateInput(session, "season", label = NULL, value = theSeason, min = NULL,
#                     max = NULL)
    
    session$output$seasonVal <- renderText({
      
      theSeason })
    
    session$output$standings <- DT::renderDataTable({
      theDivision <-  all %>% 
        filter(Season==theSeason&team==input$team) %>% 
        .$division
      
      all %>% 
        filter(Season==theSeason&division==theDivision) %>% 
        arrange(Position) %>% 
       # select(team,Pl=GP,W,D,L,Pts,GF=gf,GA=ga,GD=gd) %>%
        select(team,Pl=GP,W,D,L,GD=gd,Pts) %>% 
        
        DT::datatable(options= list(scrollY = 500,paging = FALSE, searching = FALSE,info=FALSE))
      
    })
    
    session$output$results <- DT::renderDataTable({
      df %>% 
        filter(Season==theSeason&(home==input$team|visitor==input$team)) %>% 
        mutate(result=paste(hgoal,vgoal,sep=" - ")) %>% 
        select(Date,home,result,visitor) %>% 
        arrange(Date) %>% 
        DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
      
    })
    
  }
  
  
  
  # position by year graph
  observe ({
    
    all %>% 
      
      group_by(division) %>% 
      filter(team==input$team) %>% 
      ggvis(~Season,~Overall) %>% 
      layer_points(fill=~division) %>% 
      scale_numeric("y", reverse=TRUE) %>% 
      add_axis("y",title="Overall League Position") %>% 
      add_axis("x",title="",format="####") %>% 
      handle_click(getSeason) %>% 
      set_options(height = 480, width = 480) %>% 
       bind_shiny("plot")
 })
  
  # result matrix by year
  
  output $plot_matrix <- renderPlot({
    
    df <- df  %>% filter(Season==input$year_matrix&division==input$division) %>% 
      
      select(home,visitor,FT,hgoal,vgoal) %>% 
      mutate(GD=hgoal-vgoal,
             result = ifelse(GD>0, "H", ifelse(GD<0, "A", "D"))
      )
    
    temp <-
      rbind(
        df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal),
        df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal)
      ) #rbind two copies of the orignal df, simply reversing home/away team for each match
    # win was worth 2 points until 1981
    
    if (input$year_matrix>=1981) {
      temp1<-
        temp %>%
        mutate(GD = GF-GA) %>%
        group_by(team) %>%
        summarize(GP = n(),
                  gf = sum(GF),
                  ga = sum(GA),
                  gd = sum(GD),
                  W = sum(GD>0),
                  D = sum(GD==0),
                  L = sum(GD<0)
        ) %>%
        mutate(Pts = (W*3) + D) %>%
        arrange(desc(Pts),desc(gd),desc(gf),team)
    } else {
      
      temp1<-
        temp %>%
        mutate(GD = GF-GA) %>%
        group_by(team) %>%
        summarize(GP = n(),
                  gf = sum(GF),
                  ga = sum(GA),
                  gd = sum(GD),
                  W = sum(GD>0),
                  D = sum(GD==0),
                  L = sum(GD<0)
        ) %>%
        mutate(Pts = (W*2) + D) %>%
        arrange(desc(Pts),desc(gd),desc(gf),team)
    }
    df$home <- factor(df$home, levels=rev(temp1$team))
    # levels(df$home)
    df$visitor <- factor(df$visitor, levels=temp1$team)
    #  levels(df$visitor)
    
    ggplot(df, aes(home, visitor, fill = factor(result))) + 
      geom_tile(colour="gray20", size=1.5, family="bold", stat="identity", height=1, width=1) + 
      geom_text(data=df, aes(home, visitor, label = FT), color="black", size=rel(2.5)) +
      coord_flip() +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      xlab("") + 
      ylab("") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_text(color="white", size=rel(0.8)),
        panel.background = element_rect(fill="gray20"),
        plot.background = element_rect(fill="gray20"),
        legend.position = "none",
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust=0)        
      )
    
    
  })
  
  
  # head to head 
  output$headToHead <- DT::renderDataTable({
    
    print(input$team_MU)
    
    home <- df %>% 
      filter(home==input$team_MU) %>% 
      group_by(visitor) %>% 
      summarise(P = n(), GF = sum(hgoal), GA = sum(vgoal), GD=sum(goaldif),
                W=sum(result=="H"), D=sum(result=="D"), L=sum(result=="A") ) %>% 
      rename(opponent=visitor)
    away <- df %>% 
      filter(visitor==input$team_MU) %>% 
      group_by(home) %>% 
      summarise(P = n(), GF = sum(vgoal), GA = sum(hgoal), GD=GF-GA,
                W=sum(result=="A"), D=sum(result=="D"), L=sum(result=="H") )%>% 
      rename(opponent=home)
    
    total <- rbind(home,away) %>% 
      group_by(opponent) %>% 
      summarize(P=sum(P),GF=sum(GF),GA=sum(GA),GD=GF-GA,W=sum(W),D=sum(D),L=sum(L)) %>% 
      DT::datatable(rownames=FALSE,options= list(pageLength=10,paging = TRUE, searching = TRUE,info=FALSE))
    
    
    
  })
}
