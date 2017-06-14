smoothAddQ <- function(df, ObsDf, startSm, endSm) {
  
  smPeriod <- subset(datP, dateTime >= startSm & dateTime <= endSm)
  
  smPeriod$Estimated <- as.numeric(smPeriod$Estimated)
  
  smPeriod <- dplyr::left_join(x = smPeriod, y = ObsDf, by = "dateTime")
  
  if(nrow(ObsDf) == 1) {
    
    smPeriodLeft <- dplyr::filter(smPeriod, dateTime <= ObsDf$dateTime)
    
    smPeriodRght <- dplyr::filter(smPeriod, dateTime > ObsDf$dateTime)
    
    diffDatesLeft <- as.numeric(ObsDf$dateTime) - as.numeric(startSm)
    
    diffDatesRght <- as.numeric(endSm) - as.numeric(ObsDf$dateTime)
    
    midResid <- na.omit(smPeriod$Flow.obs - smPeriod$Estimated)
    
    slopeResidLeft <- (midResid - leftResid) / diffDatesLeft
    
    slopeResidRght <- (rightResid - midResid) / diffDatesRght
    
    interceptLeft <- leftResid
    
    interceptRght <- midResid
    
    adjResidLeft <- interceptLeft + slopeResidLeft*(as.numeric(smPeriodLeft$dateTime) - as.numeric(startSm))
    
    adjResidRght <- interceptRght + slopeResidRght*(as.numeric(smPeriodRght$dateTime) - as.numeric(ObsDf$dateTime))
    
    adjResidVec <- c(adjResidLeft, adjResidRght)
    
    smPeriod$adjResid <- adjResidVec
    
    smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid
    
    smPeriod <- data.frame(dateTime = smPeriod$dateTime, Smoothed = signif(smPeriod$Smoothed, 3), 
                           adjResid = signif(smPeriod$adjResid, 3))
    
  }
  
  else if(nrow(ObsDf) > 1) {
    
    adjResidList <- list()
    
    ObsDfAdd <- dplyr::left_join(x = ObsDf, y = smPeriod, by = "dateTime")
    
    ObsDfAdd <- ObsDfAdd[,c(1:12)]
    
    for(i in seq(1, (nrow(ObsDf) + 1), 1)) {
      
      if(i == 1) {
        
        smPeriodLeft <- dplyr::filter(smPeriod, dateTime <= ObsDf[i, 1])
        
        diffDatesLeft <- as.numeric(ObsDf[i, 1]) - as.numeric(startSm)
        
        midResid <- ObsDfAdd[i, 2] - ObsDfAdd[i, 12]
        
        slopeResidLeft <- (midResid - leftResid) / diffDatesLeft
        
        adjResidList[[i]] <- leftResid + slopeResidLeft*(as.numeric(smPeriodLeft$dateTime) - as.numeric(startSm))
        
      }
      
      else if(i > 1 & i < (nrow(ObsDf) + 1)) {
        
        smPeriodNew <- dplyr::filter(smPeriod, dateTime >= ObsDf[i - 1, 1] & dateTime <= ObsDf[i, 1])
        
        diffDatesNew <- as.numeric(smPeriodNew[nrow(smPeriodNew),1]) - as.numeric(smPeriod[1,1])
        
        midResidLeft <- ObsDfAdd[i - 1, 2] - ObsDfAdd[i - 1, 12]
        
        midResidRght <- ObsDfAdd[i, 2] - ObsDfAdd[i, 12]
        
        slopeResidNew <- (midResidRght - midResidLeft) / diffDatesNew
        
        adjResidNew <- midResidLeft + slopeResidNew*(as.numeric(smPeriodNew$dateTime) - as.numeric(ObsDf[i - 1, 1]))
        
        adjResidNew <- adjResidNew[-1]
        
        adjResidList[[i]] <- adjResidNew
        
      }
      
      else if(i == (nrow(ObsDf) + 1)) {
        
        smPeriodNew <- dplyr::filter(smPeriod, dateTime >= ObsDf[i - 1, 1])
        
        diffDatesNew <- as.numeric(endSm) - as.numeric(ObsDf[i - 1, 1])
        
        midResidLeft <- ObsDfAdd[i - 1, 2] - ObsDfAdd[i - 1, 12]
        
        slopeResidNew <- (rightResid - midResidLeft) / diffDatesNew
        
        adjResidNew <- midResidLeft + slopeResidNew*(as.numeric(smPeriodNew$dateTime) - as.numeric(ObsDf[i - 1, 1]))
        
        adjResidNew <- adjResidNew[-1]
        
        adjResidList[[i + 1]] <- adjResidNew
        
      }
      
    }
    
    adjResidAddQs <- unlist(adjResidList)
    
    smPeriod$adjResid <- adjResidAddQs
    
    smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid
    
    smPeriod <- data.frame(dateTime = smPeriod$dateTime, Smoothed = signif(smPeriod$Smoothed, 3), 
                           adjResid = signif(smPeriod$adjResid, 3))
    
  }
  
  datP <- dplyr::left_join(x = datP, y = smPeriod, by = "dateTime")
  
  return(datP)
  
}
