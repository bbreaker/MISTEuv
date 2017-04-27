output$plotResids <- renderPlot({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(regDat, estDat)
  
  if (input$eventEst == FALSE) {
    
    if (input$use2 == FALSE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
        
        regDF <- augment(regObj)
        
        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) + 
          geom_point() + 
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.99, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()
        
        p
          
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
        
        regDF <- augment(regObj)
        
        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) + 
          geom_point() + 
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.99, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()
        
        p
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
        
        regDF <- augment(regObj)
        
        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) + 
          geom_point() + 
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.99, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()
        
        p
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
        
        regDF <- augment(regObj)
        
        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) + 
          geom_point() + 
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.99, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()
        
        p
        
      }
      
    }
    
  }
  
  else if (input$eventEst == TRUE) {
    
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
    
    regObjRise <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDatRise, select = TRUE)
    
    regObjFall <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDatFall, select = TRUE)
    
    regDFRise <- augment(regObjRise)
    
    regDFRise$comment <- "rise"
    
    regDFFall <- augment(regObjFall)
    
    regDFFall$comment <- "fall"
    
    regDF <- dplyr::bind_rows(regDFRise, regDFFall)
    
    p <- ggplot(regDF, aes(x = .fitted, y = .resid)) + 
      geom_point() + 
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
      stat_smooth(method = "loess", span = 0.99, color = "red", linetype = "dashed", se = FALSE) +
      scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
      labs(x = "Fitted values", y = "Residuals") +
      facet_wrap(~comment) +
      theme_bw()
    
    p
    
  }
  
})