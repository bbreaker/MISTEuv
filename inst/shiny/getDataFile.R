output$downloadData <- downloadHandler(
  
  filename = function() {paste0("data", input$yID, ".mud")},
  
  content =  function(file) {
    
    estDat <- getEstDat()
    
    regDat <- getRegDat()
    
    datP <- allMISTEdat(estDat, regDat)
    
    DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)
    
  }
  
  write.csv(datP, file)
  
)