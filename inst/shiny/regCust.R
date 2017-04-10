output$regCust <- renderPrint({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat()
  
  if (input$use2 == FALSE) {
    
    if (input$Method == 1) {
      
      regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
      
      newSummary <- list()
      
      newSummary[[1]] <- paste0("Index site ", input$xID, " was used to estimate data for ", input$yID)
      
      newSummary
      
    }
    
    else if (input$Method == 2) {
      
      regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
      
      newSummary <- list()
      
      newSummary[[1]] <- paste0("Index site ", input$xID, " was used to estimate data for ", input$yID)
      
      newSummary
      
    }
    
  }
  
  else if (input$use2 == TRUE) {
    
    if (input$Method == 1) {
      
      regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
      
      newSummary <- list()
      
      newSummary[[1]] <- paste0("Index sites ", input$xID, " and ", input$xID2, " was used to estimate data for ", input$yID)
      
      newSummary
      
    }
    
    else if (input$Method == 2) {
      
      regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                      s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
      
      newSummary <- list()
      
      newSummary[[1]] <- paste0("Index sites ", input$xID, " and ", input$xID2, " was used to estimate data for ", input$yID)
      
      newSummary
      
    }
    
  }
  
})