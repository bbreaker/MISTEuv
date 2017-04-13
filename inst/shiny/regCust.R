output$regCust <- renderPrint({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(estDat, regDat)
  
  newNames <- c("Estimates for", "Index 1", "Lag for Index 1", "Index 2", "Lag for Index 2", "Summary", "Method", "Adj R-squared", 
                "Adj R-squared rise", "Adj R-squared fall", "Num of smooth terms", 
                "Num of smooth terms fall", "Num of smooth term rise", "Regression range", 
                "Estimation range", "Smoothing applied", "Smoothing date range", "Peak input", "Peak input_date")
  
  if (input$eventEst == FALSE) {
    
    if (input$use2 == FALSE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
        
        newSummary <- c(input$yID, 
                        input$xID, 
                        input$lag1,
                        "", 
                        "",
                        "steady", 
                        "linear regression", 
                        summary(regObj)$adj.r.squared, 
                        "", 
                        "", 
                        "", 
                        "", 
                        "", 
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$smthDateSt, " to ", input$smthDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate))
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
        
        newSummary <- c(input$yID, 
                        input$xID, 
                        input$lag1,
                        "", 
                        "",
                        "steady", 
                        "gam regression", 
                        summary(regObj)$r.sq, 
                        "", 
                        "", 
                        summary(regObj)$m, 
                        "", 
                        "", 
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$smthDateSt, " to ", input$smthDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate))
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
        
        newSummary <- c(input$yID, 
                        input$xID, 
                        input$lag1,
                        input$xID2, 
                        input$lag2,
                        "steady", 
                        "linear regression", 
                        summary(regObj)$adj.r.squared, 
                        "", 
                        "", 
                        "", 
                        "", 
                        "", 
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$smthDateSt, " to ", input$smthDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate))
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
        
        newSummary <- c(input$yID, 
                        input$xID, 
                        input$lag1,
                        input$xID2, 
                        input$lag2,
                        "steady", 
                        "gam regression", 
                        summary(regObj)$r.sq, 
                        "", 
                        "", 
                        summary(regObj)$m, 
                        "", 
                        "", 
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$smthDateSt, " to ", input$smthDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate))
        
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
    
    regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
    
    newSummary <- c(input$yID, 
                    input$xID, 
                    input$lag1,
                    "", 
                    "",
                    "event", 
                    "gam regression", 
                    "", 
                    summary(regObjRise)$r.sq, 
                    summary(regObjFall)$r.sq, 
                    "", 
                    summary(regObjRise)$m, 
                    summary(regObjFall)$m, 
                    paste0(input$regDateSt, " to ", input$regDateEn),
                    paste0(input$estDateSt, " to ", input$estDateEn),
                    dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                    dplyr::if_else(input$smooth == FALSE, "", paste0(input$smthDateSt, " to ", input$smthDateEn)),
                    dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                    dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate))
    
  }
  
  cat(paste0(newNames, " = ", newSummary, "\n"))
  
})
