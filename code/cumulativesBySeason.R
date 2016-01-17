
dataCum <- reactive({
  
  if(is.null(input$cumteam)) return()
  
  
  
  df_all <-temp %>% 
    group_by(Season,team) %>% 
    arrange(Date) %>% 
    mutate(gameOrder=row_number(),cumGA=cumsum(GA),cumGF=cumsum(GF),cumG=cumGA+cumGF,cumGD=cumGF-cumGA) %>% 
    filter(team==input$cumteam) %>% 
    mutate(tier=as.character(tier)) %>% 
    ungroup()
  
  ## highl;ight premYears - if in and current year
  
  premYears <- df_all %>% 
    filter(Season>1991&tier==1) %>% 
    select(Season)
  
  premYears <- unique(premYears$Season)
  
  
  df_all$grp <- ifelse(df_all$Season ==2015, 2, ifelse(df_all$Season %in% premYears,1,0)) #not quite right
  
  
  df_prem <- df_all %>%  
    filter(Season %in% premYears&tier==1)
  
  df_2015 <- df_all %>%  
    filter(Season=="2015") 
  
  test <- df_all %>% 
    filter(grp==1)
  
  print(glimpse(df_all))
  
  # for prem years want to exclude non_prem
  df_nonPrem <- df_all %>% 
    anti_join(df_prem) 
  print(glimpse(df_all))
  print(glimpse(df_2015))
  
  
  info= list(df_all=df_all,df_2015=df_2015,df_prem=df_prem,df_nonPrem=df_nonPrem)
  return(info)
  
})


  
  

  

  
output$cumulativePlot <- renderPlot({
  if(is.null(input$cumteam)) return()
  
 

  df_all <- dataCum()$df_all
  df_prem <- dataCum()$df_prem
  df_nonPrem <- dataCum()$df_nonPrem
  df_2015 <- dataCum()$df_2015

  if (input$cumulative=="For") {
 theTitle <- paste0(input$cumteam," - League Goals For")
basePlot <-  ggplot(df_nonPrem, aes(gameOrder, cumGF, group=Season, color=grp))  +
  geom_line(aes(group=Season, color=factor(grp))) +
  geom_line(data=df_prem, aes(gameOrder, cumGF, group=Season, color=factor(grp))) +  
  geom_line(data=df_2015, aes(gameOrder, cumGF, group=Season, color=factor(grp)), lwd=1.1)
  } else if (input$cumulative=="Ag") {
  theTitle <- paste0(input$cumteam," - League Goals Against")
  basePlot <-  ggplot(df_nonPrem, aes(gameOrder, cumGA, group=Season, color=grp))  +
    geom_line(aes(group=Season, color=factor(grp))) +
    geom_line(data=df_prem, aes(gameOrder, cumGA, group=Season, color=factor(grp))) +  
    geom_line(data=df_2015, aes(gameOrder, cumGA, group=Season, color=factor(grp)), lwd=1.1)
  } else if (input$cumulative=="Diff") {
    print("enter diff")
    theTitle <- paste0(input$cumteam," - League Goals Difference")
    basePlot <-  ggplot(df_nonPrem, aes(gameOrder, cumGD, group=Season, color=grp))  +
      geom_line(aes(group=Season, color=factor(grp))) +
      geom_line(data=df_prem, aes(gameOrder, cumGD, group=Season, color=factor(grp))) #+  
    #  geom_line(data=df_2015, aes(gameOrder, cumGD, group=Season, color=factor(grp)), lwd=1.1)
    
} else if (input$cumulative=="Total Both Teams") {
  theTitle <- paste0(input$cumteam," - League Goals Total in Game")
  basePlot <-  ggplot(df_nonPrem, aes(gameOrder, cumG, group=Season, color=grp))  +
    geom_line(aes(group=Season, color=factor(grp))) +
    geom_line(data=df_prem, aes(gameOrder, cumG, group=Season, color=factor(grp))) +  
    geom_line(data=df_2015, aes(gameOrder, cumG, group=Season, color=factor(grp)), lwd=1.1)
}
basePlot +
  
    xlab("Games Played") + ylab("Cumulative Goals") +
    scale_color_manual(values=c("gray80","#F9966B" , "blue")) +
    scale_x_continuous(breaks=seq(from=1,to=42, by=5)) +
    labs(title=theTitle) +
    theme(
      plot.title = element_text(hjust=0,vjust=1, size=rel(1.7)),
      panel.background = element_blank(),
      panel.grid.major.y = element_line(color="gray65"),
      panel.grid.major.x = element_line(color="gray65"),
      panel.grid.minor = element_blank(),
      plot.background  = element_blank(),
      text = element_text(color="gray20", size=10),
      axis.text = element_text(size=rel(1.0)),
      axis.text.x = element_text(color="gray20",size=rel(1.5)),
      axis.text.y = element_text(color="gray20", size=rel(1.5)),
      axis.title.x = element_text(size=rel(1.5), vjust=0),
      axis.title.y = element_text(size=rel(1.5), vjust=1),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )
  
})



output$cumulativeGameOrderPlot <- renderPlotly({
  if(is.null(input$cumteam)) return()
  df_all <- dataCum()$df_all
#   print(glimpse(df_all))
#   write_csv(df_all,"probs.csv")
#  cols <- sort(RColorBrewer::brewer.pal(nlevels(as.factor(df_all$tier)), "BrBG")) #cols ""#D8B365" "#F5F5F5" "#5AB4AC" nlevels(df_all$tier)=0 so add factor
  
  #print(cols)
  df1 <- subset(df_all,gameOrder==17)
  plotTitle <- paste0(input$cumteam, " - Goal Difference by Games Played  ",
                     " <br> Line indicates most recent round
                      <br> Zoom and Pan as desired")
  
  p <-plot_ly(df_all, x = Season, y = jitter(cumGD), mode = "markers", color = tier,#colors=cols,
              marker=list(size=3), hoverinfo="text",
              text = paste0("Tier: ",tier,
                            "<br>Season: ",Season,
                            "<br>Played: ",gameOrder,
                            "<br>GD: ",cumGD
              )) %>%
    layout(hovermode = "closest",
           showlegend = F,
           title=plotTitle,
           xaxis=list(title=""),
           yaxis=list(title="Cumulative Goal Difference"
           )
    )
  #lineCol <- "#E41A1C" # is a red 
  
  p %>%
    add_trace(x=df1$Season,y=df1$cumGD, mode="line", line=list(color="#E41A1C"))
  
 
})