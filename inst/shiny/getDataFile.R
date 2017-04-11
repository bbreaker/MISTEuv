output$downloadData <- downloadHandler(
  
  filename = function() {paste0("data", input$yID, ".mud")},
  
  content =  function(file) {
    
    estDat <- getEstDat()
    
    regDat <- getRegDat()
    
    datP <- allMISTEdat(estDat, regDat)
    
    if(input$smooth == FALSE) {
      
      datP <- datP %>%
        dplyr::filter(is.na(Flow.y)) %>%
        dplyr::select(dateTime, Flow = Estimated)
      
    }
    
    else if(input$smooth == TRUE) {
      
      datP <- datP %>%
        dplyr::filter(is.na(Flow.y)) %>%
        dplyr::select(dateTime, Flow = Smoothed)
      
    }
    
    write.csv(datP, file, row.names = FALSE)
    
  }

)