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
    
    print(glimpse(df))
    print(str(df))
    
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
  print("enter inputteam")
  print(input$team)
  print("shows inputteam")
  if (is.null(input$team)) return()
selection<-  all %>% 
    
   # group_by(tier) %>% # was division
    filter(team==input$team) 
## try arranging by year
    
  


selection  <- cbind(selection, id = seq_len(nrow(selection)))  

  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- selection[selection$id == x$id,c("Season","tier","Position")]
    paste0(names(row),": ", format(row), collapse = "<br />")
  }
 
  print(glimpse(selection))
   
 test <- selection %>% 
    
  #  group_by(tier) %>% # was division with above seems to obviate disappearing points
    filter(team==input$team)
 
 print(View(test))
 
 test %>% 
    ggvis(~Season,~Overall,key := ~id) %>% 
    layer_points(fill=~tier) %>% 
    scale_numeric("y", reverse=TRUE) %>% 
    add_axis("y",title="Overall League Position") %>% 
    add_axis("x",title="",format="####") %>% 
    add_tooltip(all_values,"click") %>% 
    handle_click(getSeason) %>% 
    set_options(height = 480, width = 480) %>% 
    bind_shiny("plot")
})
## issue with plot showing and then immediately all but lowest divisions disappear

