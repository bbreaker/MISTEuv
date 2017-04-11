getBestOffset <- function(regDat){
  
  df <- regDat
  
  offsetDat <- data.frame(offset = as.numeric(), correlation = as.numeric())
  
  for (i in seq(-1440, 1440, 15)) {
    
    datOffset <- df
    
    if(i >= 0) {
      
      datOffset$Flow.x <- dplyr::lead(datOffset$Flow.x, i/15)
      
    }
    
    else if(i < 0) {
      
      datOffset$Flow.x <- dplyr::lag(datOffset$Flow.x, abs(i)/15)
      
    }
    
    corNew <- tryCatch({
      
      summary(lm(log10(Flow.y) ~ log10(Flow.x), data = datOffset))$adj.r.squared
      
    },
    
    error = function(cond){
      
      0
      
    })
    
    offsetDatNew <- data.frame(offset = i, correlation = corNew)
    
    offsetDat <- dplyr::bind_rows(offsetDat, offsetDatNew); rm(offsetDatNew)
    
  }
  
  bestOffset <- max(offsetDat$correlation, na.rm = TRUE)
  
  offsetDatNew <- offsetDat[which(offsetDat$correlation == bestOffset),]
  
  return(offSetDatNew$offset)
  
}