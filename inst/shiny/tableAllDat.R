output$tableAllDat <- DT::renderDataTable({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(estDat, regDat)
  
  DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)
  
})