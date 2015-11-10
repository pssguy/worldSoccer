



output$cumulativePlot <- renderPlot({
  
  if(is.null(input$team)) return()
  
  print("enter cum")
  print(input$team)
  
  df_all <-temp %>% 
    group_by(Season,team) %>% 
    arrange(Date) %>% 
    mutate(gameOrder=row_number(),cumGA=cumsum(GA),cumGF=cumsum(GF),cumG=cumGA+cumGF) %>% 
    filter(team==input$team) %>% 
    ungroup()
  #  group_by(Season)  # highest was 120 years ago
  
  # groups for cooloring
 
  
  ## highl;ight premYears - if in and current year
  
  premYears <- df_all %>% 
    filter(Season>1991&tier==1) %>% 
    select(Season)
  
  premYears <- unique(premYears$Season)
  print(premYears)
  
#   df_all$grp <- ifelse(df_all$Season <1992, 0, ifelse(df_all$Season %in% premYears,1,0))
#   df_all$grp <- ifelse(df_all$Season == 2015, 2, df_all$grp)
  
  df_all$grp <- ifelse(df_all$Season ==2015, 2, ifelse(df_all$Season %in% premYears,1,0)) #not quite right
 # df_all$grp <- ifelse(df_all$Season == 2015, 2, df_all$grp)
  
  df_prem <- df_all %>%  
    filter(Season %in% premYears&tier==1)
  
  df_2015 <- df_all %>%  
    filter(Season=="2015")
  
#   df_prem <- df_all %>% 
#     filter(Season>1991&Season<2015)
  
 theTitle <- paste0(input$team," - League Goals For and Against By Game")
 print(theTitle)
  
  ggplot(df_all, aes(gameOrder, cumG, group=Season, color=grp)) +
    geom_line(aes(group=Season, color=factor(grp))) +
    geom_line(data=df_prem, aes(gameOrder, cumG, group=Season, color=factor(grp))) +  
    geom_line(data=df_2015, aes(gameOrder, cumG, group=Season, color=factor(grp)), lwd=1.1) +
    xlab("Games Played") + ylab("Cumulative Goals by Both Teams") +
    scale_color_manual(values=c("gray80","#F9966B" , "red")) +
    scale_x_continuous(breaks=c(1:42), labels=NULL) +
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