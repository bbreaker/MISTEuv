output$plotSmth <- renderPlot({
  
  if (input$smooth == TRUE) {
    
    estDat <- getEstDat()
    
    regDat <- getRegDat()
    
    datP <- allMISTEdat()
    
    if (input$use2 == FALSE) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line() +
          geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
          geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
          geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
          geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
          geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (inputlog10 == TRUE) {
        
        p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line() +
          geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
          geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
          geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
          geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
          geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line() +
          geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
          geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
          geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
          geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
          geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
          #scale_y_log10() +
          #annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line() +
          geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
          geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
          geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
          geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
          geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  }
  
})