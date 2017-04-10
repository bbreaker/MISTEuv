applySmooth <- function(datP) {
  
  startSm <- as.POSIXct(input$smthDateSt, format = "%Y-%m-%d %H:%M:%S") 
  - as.difftime(30, units = "mins")
  
  endSm <- as.POSIXct(input$smthDateEn, format = "%Y-%m-%d %H:%M:%S") 
  + as.difftime(30, units = "mins")
  
  smPeriod <- subset(datP, dateTime >= startSm & dateTime <= endSm)
  
  allResids <- smPeriod$Flow.y - smPeriod$Estimated
  
  leftResid <- (allResids[1] + allResids[2]) / 2
   
  rightResid <- ((allResids[(length(allResids))]) + (allResids[(length(allResids) - 1)])) / 2
  
  diffDates <- as.numeric(endSm) - as.numeric(startSm)
  
  slopeResid <- (rightResid - leftResid) / diffDates
  
  intercept <- leftResid
  
  smPeriod$adjResid <- intercept + slopeResid*(as.numeric(smPeriod$dateTime) - as.numeric(startSm))
  
  smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid
  
  smPeriod <- data.frame(dateTime = smPeriod$dateTime, Smoothed = signif(smPeriod$Smoothed, 3), 
                         adjResid = signif(smPeriod$adjResid, 3))
  
  datP <- merge(x = datP, y = smPeriod, by = "dateTime", all.x = FALSE)
  
  return(datP)
  
}