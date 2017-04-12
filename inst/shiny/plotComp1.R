output$plotComp1 <- renderPlot({
  
  regDat <- getRegDat()
  
  bestOffsetVal <- getBestOffset(regDat) 
  
  if(input$lag1 == 0) {
    
    if (input$log10 == FALSE) {
      
      p <- ggplot() +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"), 
                 y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], tz = "GMT")) + 
        labs(x = "Date", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        scale_y_log10() +
        annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"),
                 y = 0.99*(max(regDat$Flow.x)), x = x = as.POSIXct(regDat[nrow(regDat)/2,1], tz = "GMT")) + 
        annotation_logticks(sides = "rl") +
        labs(x = "Date", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
      
    }
    
    p
    
  }
  
  else if(input$lag1 != 0) {
    
    if (input$log10 == FALSE) {
      
      p <- ggplot() +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        labs(x = "Date", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        scale_y_log10() +
        annotation_logticks(sides = "rl") +
        labs(x = "Date", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
      
    }
    
    p
    
  }
  
})