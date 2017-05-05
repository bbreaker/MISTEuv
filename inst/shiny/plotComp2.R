output$plotComp2 <- renderPlot({
  
  if (input$use2 == TRUE) {
    
    regDat <- getRegDat()
    
    bestOffsetVal <- getBestOffset2(regDat) 
    
    if(input$lag2 == 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second",
               title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y2-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second",
               title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
    else if(input$lag2 != 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y2-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  }
  
})