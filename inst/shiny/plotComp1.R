output$plotComp1 <- renderPlot({
  
  regDat <- getRegDat()
  
  bestOffsetVal <- getBestOffset(regDat) 
  
  if(input$lag1 == 0) {
    
    if (input$log10 == FALSE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat[!is.na(regDat$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
        geom_line(data = regDat[!is.na(regDat$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), size = 1, linetype = "dotdash") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second",
             title = paste0("calculated best time offset = ", bestOffsetVal, " minutes")) +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = 0, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat[!is.na(regDat$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
        geom_line(data = regDat[!is.na(regDat$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), size = 1, linetype = "dotdash") +
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
        geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat[!is.na(regDat$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
        geom_line(data = regDat[!is.na(regDat$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), size = 1, linetype = "dotdash") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = 0, ymax = Inf), alpha = 0.4, color = "grey50") +
        geom_line(data = regDat[!is.na(regDat$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
        geom_line(data = regDat[!is.na(regDat$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), size = 1, linetype = "dotdash") +
        scale_y_log10() +
        annotation_logticks(sides = "rl") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
      
    }
    
    p
    
  }
  
})