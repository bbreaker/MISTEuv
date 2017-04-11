library(MISTEuv)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(dataRetrieval)
library(zoo)
library(lubridate)
library(gam)
library(mgcv)

options(DT.options = list(pageLength = 7, language = list(search = 'Filter:')))

allMISTEdat <- function(regDat, estDat) {
  
  if(input$eventEst == FALSE) {
    
    if (input$use2 == FALSE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
        
        predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x)
        
        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")
        
        datPred <- data.frame(estVals)
        
        datPred[,(1:4)] <- 10^datPred[,(1:4)]
        
        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, 
                              fitLower = datPred$fit.lwr, standardError = datPred$se.fit)
        
        datP <- cbind(estDat, datPred)
        
        if (input$smooth == TRUE) {
          
          datP <- applySmooth(datP)
          
        }
        
        return(datP)
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
        
        predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x)
        
        estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)
        
        datPred <- data.frame(estVals)
        
        upr <- datPred$fit + (2.5 * datPred$se.fit)
        
        lwr <- datPred$fit - (2.5 * datPred$se.fit)
        
        upr <- regObj$family$linkinv(upr)
        
        lwr <- regObj$family$linkinv(lwr)
        
        datPred <- data.frame(Estimated = 10^(datPred$fit), fitUpper = 10^(upr), 
                              fitLower = 10^(lwr), standardError = datPred$se.fit)
        
        datP <- cbind(estDat, datPred)
        
        if (input$smooth == TRUE) {
          
          datP <- applySmooth(datP)
          
        }
        
        return(datP)
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
        
        predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x, Flow.x2 = estDat$Flow.x2)
        
        estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")
        
        datPred <- data.frame(estVals)
        
        datPred[,(1:4)] <- 10^datPred[,(1:4)]
        
        datPred <- data.frame(Estimated = datPred$fit.fit, fitUpper = datPred$fit.upr, 
                              fitLower = datPred$fit.lwr, standardError = datPred$se.fit)
        
        datP <- cbind(estDat, datPred)
        
        if (input$smooth == TRUE) {
          
          datP <- applySmooth(datP)
          
        }
        
        return(datP)
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
        
        predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x, Flow.x2 = estDat$Flow.x2)
        
        estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)
        
        datPred <- data.frame(estVals)
        
        upr <- datPred$fit + (2.5 * datPred$se.fit)
        
        lwr <- datPred$fit - (2.5 * datPred$se.fit)
        
        upr <- regObj$family$linkinv(upr)
        
        lwr <- regObj$family$linkinv(lwr)
        
        datPred <- data.frame(Estimated = 10^(datPred$fit), fitUpper = 10^(upr), 
                              fitLower = 10^(lwr), standardError = datPred$se.fit)
        
        datP <- cbind(estDat, datPred)
        
        if (input$smooth == TRUE) {
          
          datP <- applySmooth(datP)
          
        }
        
        return(datP)
        
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
    
    regObjRise <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDatRise, select = TRUE)
    
    regObjFall <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDatFall, select = TRUE)
    
    predSetRise <- data.frame(dateTime = estDatRise$dateTime, Flow.x = estDatRise$Flow.x)
    
    predSetFall <- data.frame(dateTime = estDatFall$dateTime, Flow.x = estDatFall$Flow.x)
    
    estValsRise <- mgcv::predict.gam(regObjRise, predSetRise, type = "link", se.fit = TRUE)
    
    estValsFall <- mgcv::predict.gam(regObjFall, predSetFall, type = "link", se.fit = TRUE)
    
    datPredRise <- data.frame(estValsRise)
    
    uprRise <- datPredRise$fit + (2.5 * datPredRise$se.fit)
    
    lwrRise <- datPredRise$fit - (2.5 * datPredRise$se.fit)
    
    uprRise <- regObjRise$family$linkinv(uprRise)
    
    lwrRise <- regObjRise$family$linkinv(lwrRise)
    
    datPredRise <- data.frame(Estimated = 10^(datPredRise$fit), fitUpper = 10^(uprRise), 
                              fitLower = 10^(lwrRise), standardError = datPredRise$se.fit)
    
    datPredFall <- data.frame(estValsFall)
    
    uprFall <- datPredFall$fit + (2.5 * datPredFall$se.fit)
    
    lwrFall <- datPredFall$fit - (2.5 * datPredFall$se.fit)
    
    uprFall <- regObjFall$family$linkinv(uprFall)
    
    lwrFall <- regObjFall$family$linkinv(lwrFall)
    
    datPredFall <- data.frame(Estimated = 10^(datPredFall$fit), fitUpper = 10^(uprFall), 
                              fitLower = 10^(lwrFall), standardError = datPredFall$se.fit)
    
    datPredFall <- datPredFall[-1,]
    
    datPred <- dplyr::bind_rows(datPredRise, datPredFall)
    
    datPred <- data.frame(Estimated = signif(datPred$Estimated, 3), fitUpper = signif(datPred$fitUpper, 3),
                          fitLower = signif(datPred$fitLower, 3), standardError = signif(datPred$standardError, 3))
    
    datP <- dplyr::bind_cols(estDat, datPred)
    
    return(datP)
    
  }
  
}