output$plotComp1 <- renderPlot({
  
  regDat <- getRegDat()
  
  bestOffsetVal <- getBestOffset(regDat) 
  
  if(input$lag1 == 0) {
    
    if (input$log10 == FALSE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second",
             title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        scale_y_log10() +
        annotation_logticks(sides = "rl") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second", 
             title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
        theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
      
    }
    
    p
    
  }
  
  else if(input$lag1 != 0) {
    
    if (input$log10 == FALSE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
        geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        scale_y_log10() +
        annotation_logticks(sides = "rl") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
      
    }
    
    p
    
  }
  
})