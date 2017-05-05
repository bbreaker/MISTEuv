output$plotXY2 <- renderPlot({
  
  if (input$use2 == TRUE) {
    
    estDat <- getEstDat()
    
    regDat <- getRegDat()
    
    datP <- allMISTEdat(estDat, regDat)
    
    if (input$Method == 1) {
      
      p <- ggplot(data = datP, aes(x = Flow.x2, y = Flow.y)) +
        geom_point() +
        scale_y_log10() +
        scale_x_log10() +
        annotation_logticks(sides = "trbl") +
        stat_smooth(method = "lm") +
        labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2), 
             y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
        theme_bw() + theme(legend.title = element_blank())
      
      p
      
    }
    
    else if (input$Method == 2) {
      
      if (input$adjKnots == FALSE) {
        
        p <- ggplot(data = datP, aes(x = Flow.x2, y = Flow.y)) +
          geom_point() +
          scale_y_log10() +
          scale_x_log10() +
          annotation_logticks(sides = "trbl") +
          stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
          labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2), 
               y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$adjKnots == TRUE) {
        
        p <- ggplot(data = datP, aes(x = Flow.x2, y = Flow.y)) +
          geom_point() +
          scale_y_log10() +
          scale_x_log10() +
          annotation_logticks(sides = "trbl") +
          stat_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = input$knots)) +
          labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2), 
               y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
          theme_bw() + theme(legend.title = element_blank())
        
        p
        
      }
      
    }
    
  }
  
})