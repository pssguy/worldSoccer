## tables from https://en.wikipedia.org/wiki/Deloitte_Football_Money_League
## compiled into data.frame


output$deloitteData <- DT::renderDataTable({
  
  allYears %>% 
    ungroup() %>% 
    arrange(desc(Year),Rank) %>% 
    mutate(Revenue=round(Revenue)) %>% 
    select(Rank,Year,Club,Country,Revenue) %>% 
    DT::datatable(rownames=F,class='compact stripe hover row-border',
                  colnames=c("Annual Rank","Season Commencing","Club","Country","Revenue(mill Euro)"),
                  options=list(columnDefs = list(list(className = 'dt-center', targets = 1)))
                  )
  
})

observe({
  
 
  if(input$delCount=="Top 10") {
temp <-  allYears %>% 
    ungroup() %>% 
    filter(Rank<11)  
  } else if(input$delCount=="Top 20") {
    temp <-  allYears %>% 
      ungroup() %>% 
      filter(Rank<21&Year>=2004)
  } else {
    temp <-  allYears %>% 
      ungroup() %>% 
      filter(Rank<31&Year>=2009)
}
temp %>% 
    group_by(Country,Year) %>% 
    tally() %>% 
    rename(Count=n) %>% 
     ggvis(~Year,~Count) %>% 
     layer_bars(fill=~Country) %>% 
     add_axis("x", format='d', title="Season Commencing") %>% 
     add_axis("y",  title="Countries in Top 10") %>% 
   add_legend("fill", title="") %>% 
    bind_shiny("delByCountry")
})

output$delTeamCount <- DT::renderDataTable({
  
  if(input$delCount=="Top 10") {
df <-  allYears %>% 
    ungroup() %>% 
    filter(Rank<11)
  } else if(input$delCount=="Top 20") {
df <-    allYears %>% 
      ungroup() %>% 
      filter(Rank<21&Year>=2004)
  } else {
 df <-   allYears %>% 
      ungroup() %>% 
      filter(Rank<31&Year>=2009)
  }
 df   %>% 
    group_by(Club) %>% 
    tally() %>% 
    rename(Years=n) %>% 
    ungroup() %>% 
    arrange(desc(Years)) %>% 
    DT::datatable(rownames=F,class='compact stripe hover row-border',
                  options = list(searching= FALSE, info=FALSE, paging=TRUE))
  
})


observe({
  
  if (input$delCategory=="Rank") {
  allYears %>% 
    filter(Club %in% topTen) %>% 
    ggvis(~Year,~Rank) %>% 
    group_by(Club) %>% 
    layer_lines() %>% 
    layer_points(fill=~Club) %>% 
    add_axis("x", format='d', title="Season Commencing") %>% 
    scale_numeric("y", reverse=T) %>% 
    add_legend("fill",values=topTen, title="") %>% 
    bind_shiny("delTopTen")
  } else if (input$delCategory=="Revenue") {
    allYears %>% 
      filter(Club %in% topTen) %>% 
      ggvis(~Year,~Revenue) %>% 
      group_by(Club) %>% 
      layer_lines() %>% 
      layer_points(fill=~Club) %>%  
      add_axis("x", format='d', title="Season Commencing") %>% 
      add_axis("y", format='d', title="Revenue (mill. Euro)") %>% 
      add_legend("fill",values=topTen, title="") %>% 
      bind_shiny("delTopTen")
  } else {
    index <-  allYears %>% 
      filter(Club %in% topTen&Year==2011) %>% 
      mutate(baseRev=Revenue) %>% 
      select(Club,baseRev)
    
    allYears %>% 
      filter(Club %in% topTen) %>% 
      left_join(index) %>% 
      mutate(index=round(100*Revenue/baseRev)) %>% 
      ggvis(~Year,~index) %>% 
      group_by(Club) %>% 
      layer_lines() %>% 
      layer_points(fill=~Club) %>% 
      add_axis("x", format='d', title="Season Commencing") %>% 
      add_axis("y", format='d', title="Revenue Index (2004=100") %>% 
      scale_numeric("y") %>% 
      add_legend("fill",values=topTen, title="") %>% 
      bind_shiny("delTopTen")
  }
})