# function to get Season data from graph and apply to table
# getSeason = function(data,location,session){
#   
#   if(is.null(data)) return(NULL)  ## NB reactive data is in another file hence need for different names?
#   
#   theSeason <- data$Season
#   session$output$seasonVal <- renderText({
#     
#     theSeason })
#   
#   session$output$standings <- DT::renderDataTable({
#     theDivision <-  all %>% 
#       filter(Season==theSeason&team==input$team) %>% 
#       .$division
#     
#     all %>% 
#       filter(Season==theSeason&division==theDivision) %>% 
#       arrange(Position) %>% 
#       
#       select(team,Pl=GP,W,D,L,GD=gd,Pts) %>% 
#       
#       DT::datatable(options= list(scrollY = 500,paging = FALSE, searching = FALSE,info=FALSE))
#     
#   })
#   
#   session$output$results <- DT::renderDataTable({
#     
#  
#     
#     df %>% 
#       filter(Season==theSeason&(home==input$team|visitor==input$team)) %>% 
#       mutate(result=paste(hgoal,vgoal,sep=" - ")) %>% 
#       select(Date,home,result,visitor) %>% 
#       arrange(Date) %>% 
#       DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
#     
#   })
#   
# }



# position by year graph
# observe ({
# #  observeEvent(input$team,{
# #   print("inputteam")
# #   print(input$team)
#   #if (is.null(input$team)) return()
# req(input$team)
#   
# selection<-  all %>% 
#     
#    # group_by(tier) %>% # was division
#     filter(team==input$team) 
# 
# selection  <- cbind(selection, id = seq_len(nrow(selection)))  
# 
#   all_values <- function(x) {
#     if(is.null(x)) return(NULL)
#     row <- selection[selection$id == x$id,c("Season","tier","Position")]
#     paste0(names(row),": ", format(row), collapse = "<br />")
#   }
#  
#  
#    
#  test <- selection %>% 
#     
#   #  group_by(tier) %>% # was division with above seems to obviate disappearing points
#     filter(team==input$team)
#  
# # print(View(test))
#  
#  test %>% 
#     ggvis(~Season,~Overall,key := ~id) %>% 
#     layer_points(fill=~tier) %>% 
#     scale_numeric("y", reverse=TRUE) %>% 
#     add_axis("y",title="Overall League Position") %>% 
#     add_axis("x",title="",format="####") %>% 
#     add_tooltip(all_values,"click") %>% 
#     handle_click(getSeason) %>% 
#     set_options(height = 480, width = 480) %>% 
#     bind_shiny("plot")
# })
## issue with plot showing and then immediately all but lowest divisions disappear? solved

output$standingsPlot <- renderPlotly({
  
  req(input$team)
  selection<-  all %>% 
    filter(team==input$team) 
  
  
  
  ## may need to set colors in data.frame 
  p <-plot_ly(selection, x = Season, y = Overall, mode = "markers",
              color=tier,
              marker=list(size=6), hoverinfo="text", # works but colors are red and grey
              text = paste0("Season: ",Season,
                            
                            "<br>Tier: ",tier,
                            "<br>Position: ",Overall
              )) %>%
    layout(hovermode = "closest",
           showlegend = F,
           # title=plotTitle,  
           xaxis=list(title=""),
           #yaxis=list(title="Overall League Position", autorange="FALSE", range=c(92,1) an alternative
           yaxis=list(title="Overall League Position", autorange="reversed", zeroline =FALSE
           )
    )
  p
})

output$standingsTable <- DT::renderDataTable({
  req(input$team)
  s <- event_data("plotly_click")
  print(("printing s"))
  print(length(s))
  if (length(s) == 0) {
    return()
  } else {
    theSeason = as.integer(s[["x"]]) 
  
    # get tier so print correct table
    theDivision <-  all %>% 
      filter(Season==theSeason&team==input$team) %>% 
      .$division
    
    all %>% 
      filter(Season==theSeason&division==theDivision) %>% 
      arrange(Position) %>% 
      
      select(team,Pl=GP,W,D,L,GD=gd,Pts) %>% 
      
      DT::datatable(options= list(scrollY = 500,paging = FALSE, searching = FALSE,info=FALSE))
  }
})


output$results <- DT::renderDataTable({
  
  req(input$team)
  s <- event_data("plotly_click")
  print(("printing s"))
  print(length(s))
  if (length(s) == 0) {
    return()
  } else {
    theSeason = as.integer(s[["x"]]) 
  
  df %>% 
    filter(Season==theSeason&(home==input$team|visitor==input$team)) %>% 
    mutate(result=paste(hgoal,vgoal,sep=" - ")) %>% 
    select(Date,home,result,visitor) %>% 
    arrange(Date) %>% 
    DT::datatable(options= list(paging = FALSE, searching = FALSE,info=FALSE))
  }
})