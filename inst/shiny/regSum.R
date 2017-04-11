output$regSum <- renderPrint({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(estDat, regDat)
  
  if(input$eventEst == FALSE) {
    
    if (input$use2 == FALSE) {
      
      if (input$Method == 1) {
        
        summary(lm(log10(Flow.y) ~ log10(Flow.x), data = regDat))
        
      }
      
      else if (input$Method == 2) {
        
        summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE))
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        summary(lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat))
        
      }
      
      else if (input$Method == 2) {
        
        summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + s(log10(Flow.x2), bs = "ts"), 
                    data = regDat, select = TRUE))
        
      }
      
    }
    
  }
  
})