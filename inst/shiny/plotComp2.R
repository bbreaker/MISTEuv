output$plotComp2 <- renderPlot({
  
  if (input$use2 == TRUE) {
    
    estDat <- getEstDat()
    
    regDat <- getRegDat()
    
    datP <- allMISTEdat()
    
    bestOffsetVal <- getBestOffset(datP) 
    
    if(input$lag2 == 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"), 
                   y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], format = "%Y-%m-%d %H:%M:%S")) +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y2-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          scale_y_log10() +
          annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"), 
                   y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], format = "%Y-%m-%d %H:%M:%S")) +
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
    else if(input$lag2 != 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y2-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  }
  
})