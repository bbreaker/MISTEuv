aveEstData <- function(df, n) {
  
  toMove <- n*4
  
  repVec <- seq(toMove, nrow(df), toMove)
  
  testP <- data.frame(dateTime = as.character(), Estimated = as.numeric(), 
                      fitUpper = as.numeric(), fitLower = as.numeric(), 
                      standardError = as.numeric())
  
  for(i in seq(1, length(repVec), 1)) {
    
    if (i == 1) {
      
      newTP <- data.frame(dateTime = as.character(df[round(repVec[i]/2, 0),1]),
                          Estimated = signif(mean(df[c(1:repVec[i]), 10], na.rm = TRUE), 3),
                          fitUpper = signif(mean(df[c(1:repVec[i]), 11], na.rm = TRUE), 3),
                          fitLower = signif(mean(df[c(1:repVec[i]), 12], na.rm = TRUE), 3),
                          standardError = signif(mean(df[c(1:repVec[i]), 13], na.rm = TRUE), 3))
      
    }
    
    else if (i != 1) {
      
      newTP <- data.frame(dateTime = as.character(df[(repVec[i]-(toMove/2)),1]),
                          Estimated = signif(mean(df[seq(repVec[i-1]+1, repVec[i], 1), 10], na.rm = TRUE), 3),
                          fitUpper = signif(mean(df[seq(repVec[i-1]+1, repVec[i], 1), 11], na.rm = TRUE), 3),
                          fitLower = signif(mean(df[seq(repVec[i-1]+1, repVec[i], 1), 12], na.rm = TRUE), 3),
                          standardError = signif(mean(df[seq(repVec[i-1]+1, repVec[i], 1), 13], na.rm = TRUE), 3))
      
    }
    
    testP <- bind_rows(testP, newTP); rm(newTP)
    
  }
  
  testP$dateTime <- as.POSIXct(testP$dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  return(testP)
  
}
