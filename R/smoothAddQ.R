smoothAddQ <- function(df, ObsDf, startSm, endSm, startDat, endDat) {
  
  smPeriod <- subset(df, dateTime >= startSm & dateTime <= endSm)
  
  smPeriod <- dplyr::left_join(smPeriod, ObsDf, by = "dateTime")
  
  leftResid <- dplyr::filter(smPeriod, dateTime < startDat)
  
  leftResid <- leftResid %>%
    dplyr::mutate(leftResidCol = leftResid$Flow.y - leftResid$Estimated) %>%
    dplyr::summarize(leftResidCol = mean(leftResidCol))
  
  leftResid <- leftResid$leftResidCol
  
  rghtResid <- dplyr::filter(smPeriod, dateTime > endDat) 
  
  rgthResid <- rghtResid %>%
    mutate(rghtResidCol = rghtResid$Flow.y - rghtResid$Estimated) %>%
    summarize(rghtResidCol = mean(rghtResidCol))
  
  rghtResid <- rghtResid$rghtResidCol
  
  if(nrow(ObsDf) == 1) {
    
    smPeriodLeft <- dplyr::filter(smPeriod, dateTime <= ObsDf$dateTime)
    
    smPeriodRght <- dplyr::filter(smPeriod, dateTime > ObsDf$dateTime)
    
    diffDatesLeft <- as.numeric(ObsDf$dateTime) - as.numeric(startDat)
    
    diffDatesRght <- as.numeric(endDat) - as.numeric(ObsDf$dateTime)
    
    midResid <- na.omit(smPeriod$Flow.obs - smPeriod$Estimated)
    
    slopeResidLeft <- (midResid - leftResid) / diffDatesLeft
    
    slopeResidRght <- (rghtResid - midResid) / diffDatesRght
    
    interceptLeft <- leftResid
    
    interceptMid <- midResid
    
    adjResidLeft <- interceptLeft + slopeResidLeft*(as.numeric(smPeriodLeft$dateTime) - as.numeric(startDat))
    
    adjResidRght <- interceptMid + slopeResidRght*(as.numeric(smPeriodRght$dateTime) - as.numeric(ObsDf$dateTime))
    
    adjResidVec <- c(adjResidLeft, adjResidRght)
    
    smPeriod$adjResid <- adjResidVec
    
    smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid
    
    smPeriod <- data.frame(dateTime = smPeriod$dateTime, Smoothed = signif(smPeriod$Smoothed, 3), 
                           adjResid = signif(smPeriod$adjResid, 3))
    
  }
  
  else if(nrow(ObsDf) > 1) {
    
    adjResidList <- list()
    
    for(i in seq(1, (nrow(ObsDf) + 1), 1)) {
      
      if(i == 1) {
        
        smPeriodLeft <- dplyr::filter(smPeriod, dateTime <= ObsDf[i, 1] & dateTime >= startDat)
        
        diffDatesLeft <- as.numeric(ObsDf[i, 1]) - as.numeric(startDat)
        
        midResid <- smPeriodLeft[nrow(smPeriodLeft),]
        
        midResid <- midResid$Flow.obs - midResid$Estimated
        
        slopeResidLeft <- (midResid - leftResid) / diffDatesLeft
        
        adjResidList[[i]] <- leftResid + slopeResidLeft*(as.numeric(smPeriodLeft$dateTime) - as.numeric(startDat))
        
      }
      
      else if(i > 1 & i < (nrow(ObsDf) + 1)) {
        
        smPeriodNew <- dplyr::filter(smPeriod, dateTime >= ObsDf[i - 1, 1] & dateTime <= ObsDf[i, 1])
        
        diffDatesNew <- as.numeric(smPeriodNew[nrow(smPeriodNew),1]) - as.numeric(smPeriodNew[1,1])
        
        midResidLeft <- smPeriodNew[1, 15] - smPeriodNew[1, 11]
        
        midResidRght <- smPeriodNew[nrow(smPeriodNew), 15] - smPeriodNew[nrow(smPeriodNew), 11]
        
        slopeResidNew <- (midResidRght - midResidLeft) / diffDatesNew
        
        adjResidNew <- midResidLeft + slopeResidNew*(as.numeric(smPeriodNew$dateTime) - as.numeric(ObsDf[i - 1, 1]))
        
        adjResidNew <- adjResidNew[-1]
        
        adjResidList[[i]] <- adjResidNew
        
      }
      
      else if(i == (nrow(ObsDf) + 1)) {
        
        smPeriodNew <- dplyr::filter(smPeriod, dateTime >= ObsDf[i - 1, 1] & dateTime <= endDat)
        
        diffDatesNew <- as.numeric(endDat) - as.numeric(ObsDf[i - 1, 1])
        
        midResidLeft <- smPeriodNew[1, 15] - smPeriodNew[1, 11]
        
        slopeResidNew <- (rghtResid - midResidLeft) / diffDatesNew
        
        adjResidNew <- midResidLeft + slopeResidNew*(as.numeric(smPeriodNew$dateTime) - as.numeric(ObsDf[i - 1, 1]))
        
        adjResidNew <- adjResidNew[-1]
        
        adjResidList[[i]] <- adjResidNew
        
      }
      
    }
    
    adjResidAddQs <- data.frame(dateTime = seq(from = startDat, to = endDat, by = "15 mins"),
                                adjResid = unlist(adjResidList))
    
    smPeriod <- dplyr::left_join(x = smPeriod, y = adjResidAddQs, by = "dateTime")
    
    smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid
    
    smPeriod <- data.frame(dateTime = smPeriod$dateTime, Smoothed = signif(smPeriod$Smoothed, 3), 
                           adjResid = signif(smPeriod$adjResid, 3))
    
  }
  
  datP <- dplyr::left_join(x = df, y = smPeriod, by = "dateTime")
  
  return(datP)
  
}
