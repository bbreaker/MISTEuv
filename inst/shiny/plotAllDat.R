output$plotAllDat <- renderPlot({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(regDat, estDat)
  
  if(input$eventEst == FALSE) {
    
    if (input$use2 == FALSE) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey80") +
          geom_line(data = datP[!is.na(datP$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
          geom_line(data = datP[!is.na(datP$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          geom_line(data = datP[!is.na(datP$Estimated),], aes(x = dateTime, y = Estimated, color = "Estimated"), size = 1, linetype = "longdash") +
          geom_line(data = datP[!is.na(datP$fitUpper),], aes(x = dateTime, y = fitUpper), color = "black", size = 1, linetype = "dashed") +
          geom_line(data = datP[!is.na(datP$fitLower),], aes(x = dateTime, y = fitLower), color = "black", size = 1, linetype = "dashed") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }	
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = 0, ymax = Inf), alpha = 0.4, color = "grey80") +
          geom_line(data = datP[!is.na(datP$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
          geom_line(data = datP[!is.na(datP$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          geom_line(data = datP[!is.na(datP$Estimated),], aes(x = dateTime, y = Estimated, color = "Estimated"), size = 1, linetype = "longdash") +
          geom_line(data = datP[!is.na(datP$fitUpper),], aes(x = dateTime, y = fitUpper), color = "black", size = 1, linetype = "dashed") +
          geom_line(data = datP[!is.na(datP$fitLower),], aes(x = dateTime, y = fitLower), color = "black", size = 1, linetype = "dashed") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey80") +
          geom_line(data = datP[!is.na(datP$Flow.x)], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          geom_line(data = datP[!is.na(datP$Flow.x2)], aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          geom_line(data = datP[!is.na(datP$Flow.y)], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
          geom_line(data = datP[!is.na(datP$Estimated)], aes(x = dateTime, y = Estimated, color = "Estimated"), size = 1, linetype = "longdash") +
          geom_line(data = datP[!is.na(datP$fitUpper)], aes(x = dateTime, y = fitUpper), color = "black", size = 1, linetype = "dashed") +
          geom_line(data = datP[!is.na(datP$fitLower)], aes(x = dateTime, y = fitLower), color = "black", size = 1, linetype = "dashed") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                        ymin = 0, ymax = Inf), alpha = 0.4, color = "grey80") +
          geom_line(data = datP[!is.na(datP$Flow.x)], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          geom_line(data = datP[!is.na(datP$Flow.x2)], aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
          geom_line(data = datP[!is.na(datP$Flow.y)], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
          geom_line(data = datP[!is.na(datP$Estimated)], aes(x = dateTime, y = Estimated, color = "Estimated"), size = 1, linetype = "longdash") +
          geom_line(data = datP[!is.na(datP$fitUpper)], aes(x = dateTime, y = fitUpper), color = "black", size = 1, linetype = "dashed") +
          geom_line(data = datP[!is.na(datP$fitLower)], aes(x = dateTime, y = fitLower), color = "black", size = 1, linetype = "dashed") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  }
  
  else if(input$eventEst == TRUE) {
    
    if (input$log10 == FALSE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = -Inf, ymax = Inf), alpha = 0.4, color = "grey80") +
        geom_line(data = datP[!is.na(datP$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
        geom_line(data = datP[!is.na(datP$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        geom_line(data = datP[!is.na(datP$Estimated),], aes(x = dateTime, y = Estimated, color = "Estimated"), size = 1, linetype = "longdash") +
        geom_line(data = datP[!is.na(datP$fitUpper),], aes(x = dateTime, y = fitUpper), color = "black", size = 1, linetype = "dashed") +
        geom_line(data = datP[!is.na(datP$fitLower),], aes(x = dateTime, y = fitLower), color = "black", size = 1, linetype = "dashed") +
        scale_y_continuous(label = comma) +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank())
      
    }
    
    else if (input$log10 == TRUE) {
      
      p <- ggplot() +
        geom_rect(aes(xmin = as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      xmax = as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), 
                      ymin = 0, ymax = Inf), alpha = 0.4, color = "grey80") +
        geom_line(data = datP[!is.na(datP$Flow.y),], aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID)), size = 1) +
        geom_line(data = datP[!is.na(datP$Flow.x),], aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
        geom_line(data = datP[!is.na(datP$Estimated),], aes(x = dateTime, y = Estimated, color = "Estimated"), size = 1, linetype = "longdash") +
        geom_line(data = datP[!is.na(datP$fitUpper),], aes(x = dateTime, y = fitUpper), color = "black", size = 1, linetype = "dashed") +
        geom_line(data = datP[!is.na(datP$fitLower),], aes(x = dateTime, y = fitLower), color = "black", size = 1, linetype = "dashed") +
        scale_y_log10(label = comma) +
        annotation_logticks(sides = "rl") +
        labs(x = "Date (UTC)", y = "Discharge, in cubic feet per second") +
        theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
      
    }
    
    p
    
  }
  
})