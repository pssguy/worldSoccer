


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
    
    
#     all %>% 
#       
#       group_by(tier) %>% 
#       filter(team==input$team) %>% 
#       ggvis(~Season,~Overall) %>% 
#       layer_points(fill=~tier) %>% 
#       scale_numeric("y", reverse=TRUE) %>% 
#       add_axis("y",title="Overall League Position") %>% 
#       add_axis("x",title="",format="####") %>% 
#       handle_click(getSeason) %>% 
#       bind_shiny("plot")
  })
  
#   output$standings <- DT::renderDataTable({
#   
#     
#     
#     theDivision <-  all %>% 
#       filter(Season==input$season&team==input$team) %>% 
#       .$division
#     
#     all %>% 
#       filter(Season==input$season&division==theDivision) %>% 
#       arrange(Position) %>% 
#      select(team,Pl=GP,W,D,L,Pts,GF=gf,GA=ga,GD=gd) %>% 
#      
#     DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
#     
#   })
}
