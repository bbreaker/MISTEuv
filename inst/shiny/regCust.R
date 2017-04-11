output$regCust <- renderPrint({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(estDat, regDat)
  
  if (input$eventEst == FALSE) {
    
    if (input$use2 == FALSE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
        
        newSummary <- list()
        
        newSummary[["Estimates for"]] <- input$yID
        
        newSummary[["Index 1"]] <- input$xID
        
        newSummary[["Index 2"]] <- ""
        
        newSummary[["Summary"]] <- "steady"
        
        newSummary[["Method"]] <- "linear regression"
        
        newSummary[["Adj. R-squared"]] <- summary(regObj)$adj.r.squared
        
        newSummary[["Regression range"]] <- paste0(input$regDateSt, " to ", input$regDateEn)
        
        newSummary[["Estimation range"]] <- paste0(input$estDateSt, " to ", input$estDateEn)
        
        newSummary[["Smoothing"]] <- dplyr::if_else(input$smooth == FALSE, "",
                                                    paste0(input$smthDateSt, " to ", input$smthDateEn))
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
        
        newSummary <- list()
        
        newSummary[["Estimates for"]] <- input$yID
        
        newSummary[["Index 1"]] <- input$xID
        
        newSummary[["Index 2"]] <- ""
        
        newSummary[["Summary"]] <- "steady"
        
        newSummary[["Method"]] <- "gam regression"
        
        newSummary[["Adj. R-squared"]] <- summary(regObj)$r.sq
        
        newSummary[["Num. of smooth terms"]] <- summary(regObj)$m
        
        newSummary[["Regression range"]] <- paste0(input$regDateSt, " to ", input$regDateEn)
        
        newSummary[["Estimation range"]] <- paste0(input$estDateSt, " to ", input$estDateEn)
        
        newSummary[["Smoothing"]] <- dplyr::if_else(input$smooth == FALSE, "",
                                                    paste0(input$smthDateSt, " to ", input$smthDateEn))
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
        
        newSummary <- list()
        
        newSummary <- list()
        
        newSummary[["Estimates for"]] <- input$yID
        
        newSummary[["Index 1"]] <- input$xID
        
        newSummary[["Index 2"]] <- input$xID2
        
        newSummary[["Summary"]] <- "steady"
        
        newSummary[["Method"]] <- "linear regression"
        
        newSummary[["Adj. R-squared"]] <- summary(regObj)$adj.r.squared
        
        newSummary[["Regression range"]] <- paste0(input$regDateSt, " to ", input$regDateEn)
        
        newSummary[["Estimation range"]] <- paste0(input$estDateSt, " to ", input$estDateEn)
        
        newSummary[["Smoothing"]] <- dplyr::if_else(input$smooth == FALSE, "",
                                                    paste0(input$smthDateSt, " to ", input$smthDateEn))
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
        
        newSummary <- list()
        
        newSummary[["Estimates for"]] <- input$yID
        
        newSummary[["Index 1"]] <- input$xID
        
        newSummary[["Index 2"]] <- input$xID2
        
        newSummary[["Summary"]] <- "steady"
        
        newSummary[["Method"]] <- "gam regression"
        
        newSummary[["Adj. R-squared"]] <- summary(regObj)$r.sq
        
        newSummary[["Num. of smooth terms"]] <- summary(regObj)$m
        
        newSummary[["Regression range"]] <- paste0(input$regDateSt, " to ", input$regDateEn)
        
        newSummary[["Estimation range"]] <- paste0(input$estDateSt, " to ", input$estDateEn)
        
        newSummary[["Smoothing"]] <- dplyr::if_else(input$smooth == FALSE, "",
                                                    paste0(input$smthDateSt, " to ", input$smthDateEn))
        
      }
      
    }
    
    newSummary
    
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
    
    newSummary <- list()
    
    newSummary[["Estimates for"]] <- input$yID
    
    newSummary[["Index 1"]] <- input$xID
    
    newSummary[["Index 2"]] <- ""
    
    newSummary[["Summary"]] <- "event"
    
    newSummary[["Method"]] <- "gam regression"
    
    newSummary[["Adj. R-squared rise"]] <- summary(regObjRise)$r.sq
    
    newSummary[["Adj. R-squared fall"]] <- summary(regObjFall)$r.sq
    
    newSummary[["Num. of smooth terms rise"]] <- summary(regObjRise)$m
    
    newSummary[["Num. of smooth terms fall"]] <- summary(regObjFall)$m
    
    newSummary[["Num. of smooth terms"]] <- summary(regObj)$m
    
    newSummary[["Regression range"]] <- paste0(input$regDateSt, " to ", input$regDateEn)
    
    newSummary[["Estimation range"]] <- paste0(input$estDateSt, " to ", input$estDateEn)
    
    newSummary[["Smoothing"]] <- dplyr::if_else(input$smooth == FALSE, "",
                                                paste0(input$smthDateSt, " to ", input$smthDateEn))
    
    newSummary[["Peak input"]] <- dplyr::if_else(input$peakToUse == FALSE, "", input$peakToUse)
    
    newSummary[["Peak input date"]] <- dplyr::if_else(input$peakDate == FALSE, "", input$peakDate)
    
  }
  
  newSummary
  
})