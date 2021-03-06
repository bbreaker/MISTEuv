output$plotComp2 <- renderPlot({
  
  if (input$use2 == TRUE) {
    
    regDat <- getRegDat()
    
    bestOffsetVal <- getBestOffset2(regDat) 
    
    yvar <- paste0("y-", input$yID)
    
    x2var <- paste0("x2-", input$xID2)
    
    cols = c(yvar = "blue", 
             x2var = "purple")
    
    if(input$lag2 == 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = yvar), size = 1) +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat[!is.na(regDat$Flow.x2),], aes(x = dateTime, y = Flow.x2, color = xvar), size = 1, linetype = "dotdash") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second",
               title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
          #scale_color_manual(values = cols) +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = yvar), size = 1) +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = 0, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat[!is.na(regDat$Flow.x2),], aes(x = dateTime, y = Flow.x2, color = x2var), size = 1, linetype = "dotdash") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second",
               title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
          #scale_color_manual(values = cols) +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
    else if(input$lag2 != 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = yvar), size = 1) +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat[!is.na(regDat$Flow.x2),], aes(x = dateTime, y = Flow.x2, color = x2var), size = 1, linetype = "dotdash") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          #scale_color_manual(values = cols) +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = yvar), size = 1) +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = 0, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat[!is.na(regDat$Flow.x2),], aes(x = dateTime, y = Flow.x2, color = xvar), size = 1, linetype = "dotdash") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          #scale_color_manual(values = cols) +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  }
  
})