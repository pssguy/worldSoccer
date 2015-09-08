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