getBestOffset2 <- function(regDat){
  
  df <- regDat
  
  offsetDat <- data.frame(offset = as.numeric(), correlation = as.numeric())
  
  for (i in seq(-1440, 1440, 15)) {
    
    datOffset <- df
    
    if(i >= 0) {
      
      datOffset$Flow.x2 <- dplyr::lead(datOffset$Flow.x2, i/15)
      
    }
    
    else if(i < 0) {
      
      datOffset$Flow.x2 <- dplyr::lag(datOffset$Flow.x2, abs(i)/15)
      
    }
    
    corNew <- tryCatch({
      
      summary(lm(log10(Flow.y) ~ log10(Flow.x2), data = datOffset))$adj.r.squared
      
    },
    
    error = function(cond){
      
      0
      
    })
    
    offsetDatNew <- data.frame(offset = i, correlation = corNew)
    
    offsetDat <- dplyr::bind_rows(offsetDat, offsetDatNew); rm(offsetDatNew)
    
  }
  
  bestOffset <- max(offsetDat$correlation, na.rm = TRUE)
  
  offsetDat <- offsetDat[which(offsetDat$correlation == bestOffset),]
  
  return(offsetDat$offset)
  
}