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
    
    maxQ <- max(regDat$Flow.y, na.rm = TRUE)
    
    maxQdt <- regDat[regDat$Flow.y == maxQ,]
    
    if(nrow(maxQdt) > 1) {
      
      maxQdt <- maxQdt[!is.na(maxQdt$Flow_cd.y),]
      
      maxQdt <- maxQdt[round(nrow(maxQdt)/2, 0),1]
      
    }
    
    else if(nrow(maxQdt) == 1) {
      
      maxQdt <- maxQdt[1,1]
      
    }
    
    regDatRise <- regDat[regDat$dateTime <= maxQdt,]
    
    regDatFall <- regDat[regDat$dateTime >= maxQdt,]
    
    if(input$givePeakQ == FALSE) {
      
      maxQP <- max(estDat$Flow.y, na.rm = TRUE)
      
      maxQPdt <- estDat[estDat$Flow.y == maxQP,]
      
      if(nrow(maxQPdt) > 1) {
        
        maxQPdt <- maxQPdt[!is.na(maxQPdt$Flow.y),]
        
        maxQPdt <- maxQPdt[round(nrow(maxQPdt)/2, 0),1]
        
      }
      
      else if(nrow(maxQPdt) == 1) {
        
        maxQPdt <- maxQPdt[1,1]
        
      }
      
      estDatRise <- estDat[estDat$dateTime <= maxQPdt,]
      
      estDatFall <- estDat[estDat$dateTime >= maxQPdt,]
      
    }
    
    else if(input$givePeakQ == TRUE) {
      
      maxQP <- as.numeric(input$peakToUse)
      
      maxQPdt <- as.POSIXct(input$peakDate, format = "%Y-%m-%d %H:%M:%S")
      
      estDatRise <- estDat[estDat$dateTime <= maxQPdt,]
      
      estDatFall <- estDat[estDat$dateTime >= maxQPdt,]
      
      estDatRise[nrow(estDatRise),8] <- maxQP
      
      estDatFall[1,8] <- maxQP
      
    }
    
    if(input$adjKnots == FALSE) {
      
      summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cs"), data = regDatRise, select = TRUE))
      
      summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cs"), data = regDatFall, select = TRUE))
      
    }
    
    else if(input$adjKnots == TRUE) {
      
      summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cr", k = input$knots), data = regDatRise))
      
      summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cr", k = input$knots), data = regDatFall))
      
    }
    
  }
  
})