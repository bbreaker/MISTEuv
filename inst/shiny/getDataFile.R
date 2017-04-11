output$downloadData <- downloadHandler(
  
  filename = function() {paste0("data", input$yID, ".mud")},
  
  content =  function(file) {
    
    estDat <- getEstDat()
    
    regDat <- getRegDat()
    
    datP <- allMISTEdat(estDat, regDat)
    
    write.csv(datP, file, row.names = FALSE)
    
  }

)