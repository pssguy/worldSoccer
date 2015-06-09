# function to get Season data from graph and apply to table
getSeason = function(data,location,session){
  
  if(is.null(data)) return(NULL)
  
  theSeason <- data$Season
  session$output$seasonVal <- renderText({
    
    theSeason })
  
  session$output$standings <- DT::renderDataTable({
    theDivision <-  all %>% 
      filter(Season==theSeason&team==input$team) %>% 
      .$division
    
    all %>% 
      filter(Season==theSeason&division==theDivision) %>% 
      arrange(Position) %>% 
      
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


