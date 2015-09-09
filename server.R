
function(input, output, session) {
  
  
  ## set up input menu in sidebar

  output$a <- renderUI({
   
    if (input$sbMenu=="matrix") {
     inputPanel(
      sliderInput("yearMatrix", "Choose Season (start year)", min=1870,max=2015,value=2015,sep="")
     )
    } else if (input$sbMenu=="pfaPlayers") {
    
      inputPanel(sliderInput(
        "pfaYear","Year(s)",min = 1973,max = 2015,value = c(1992,2015),sep = "", width =
          200)
      )
    
    } else if (input$sbMenu=="deloitte") {
      helpText("The Deloitte Football Money League is a ranking of football clubs by revenue generated from football operations. It is produced annually by the accountancy firm Deloitte and released in early February of each year, describing the season most recently finished")
      
    } else {
      
      inputPanel(selectInput("team","Choose Team",teamOptions,selected="Liverpool"))
    }
})
  
  
  
    output$b <- renderUI({
      
      if (input$sbMenu=="matrix") {
        
        if(is.null(input$yearMatrix)) return()
       divChoice <- seasonDiv %>% 
         filter(Season %in% input$yearMatrix)
       
       
       divOpts <- divChoice$division
       inputPanel(
       selectInput("division","Choose division",divOpts)
       )
      } else {
        return()
      }
    })
    

  

    

  
  
  
## link to detailed code by menuItem

  source("code/standings.R", local=TRUE)  
  source("code/matrix.R", local=TRUE)
  source("code/headToHead.R", local=TRUE)
  source("code/pfaPlayers.R", local=TRUE)
    source("code/deloitte.R", local=TRUE)

}
