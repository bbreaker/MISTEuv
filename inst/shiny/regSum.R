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
        
        if(input$adjKnots == FALSE) {
          
          summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cs"), data = regDat, select = TRUE))
          
        }
        
        else if (input$adjKnots == TRUE) {
          
          summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cr", k = input$knots), data = regDat))
          
        }
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        summary(lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat))
        
      }
      
      else if (input$Method == 2) {
        
        if (input$adjKnots == FALSE) {
          
          summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cs") + s(log10(Flow.x2), bs = "cs"), 
                      data = regDat, select = TRUE))
          
        }
        
        else if (input$adjKnots == TRUE) {
          
          summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cr", k = input$knots) + 
                        s(log10(Flow.x2), bs = "cr", k = input$knots), data = regDat))
          
        }
        
      }
      
    }
    
  }
  
  else if(input$eventEst == TRUE) {
    
    regDat <- mutate(regDat, event = if_else(Flow.x > lag(Flow.x, 1), "rise", "fall"))
    
    regDat$event <- if_else(is.na(regDat$event), "rise", regDat$event)
    
    estDat <- mutate(estDat, event = if_else(Flow.x > lag(Flow.x, 1), "rise", "fall"))
    
    estDat$event <- if_else(is.na(estDat$event), "rise", estDat$event)
    
    if(input$adjKnots == FALSE) {
      
      regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), by = factor(event), bs = "fs", k = 10), 
                    data = regDat, select = TRUE)
      
    }
    
    else if(input$adjKnots == TRUE) {
      
      regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), by = factor(event), bs = "fs", k = input$knots), 
                    data = regDat, select = TRUE)
      
    }
    
    summary(regObj)
    
  }
  
})