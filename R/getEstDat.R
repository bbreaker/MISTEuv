getEstDat <- function(session) {
  
  if (input$use2 == FALSE) {
    
    startDateP <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S")
    
    endDateP <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S")
    
    datyP <- tryCatch({
      
      readNWISuv(siteNumber = input$yID, startDate = format(startDateP, "%Y-%m-%d"), 
                 endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datyP <- datyP[,c(1:5)]
    
    colnames(datyP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
    
    datxP <- tryCatch({
      
      readNWISuv(siteNumber = input$xID, startDate = format(startDateP, "%Y-%m-%d"),
                 endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datxP <- datxP[,c(1:6)]
    
    colnames(datxP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd", "timeZone")
    
    datyP$Flow <- ifelse(datyP$Flow == 0, NA, datyP$Flow)
    
    datxP$Flow <- ifelse(datxP$Flow == 0, NA, datxP$Flow)
    
    colnames(datxP)[c(2,4:5)] <- paste0(colnames(datxP)[c(2,4:5)], ".x")
    
    datyP <- datyP[,-1]
    
    colnames(datyP)[c(1,3:4)] <- paste0(colnames(datyP)[c(1,3:4)], ".y")
    
    newDateP <- data.frame(dateTime = seq(from = as.POSIXct(startDateP), 
                                          to = as.POSIXct(endDateP), by = "15 min"))
    
    datxP <- merge(x = newDateP, y = datxP, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datyP <- merge(x = newDateP, y = datyP, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datyP <- datyP[,-c(1)]
    
    datxP <- datxP[,c(1,6,2:5)]
    
    datP <- cbind(datxP, datyP)
    
    adjustLag1 <- input$lag1/15
    
    if (adjustLag1 == 0) {
      
      return(datP)
      
    }
    
    else if (adjustLag1 > 0) {
      
      absLag1 <- abs(adjustLag1)
      
      datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag1)
      
      return(datP)
      
    }
    
    else if (adjustLag1 < 0) {
      
      absLag1 <- abs(adjustLag1)
      
      datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag1)
      
      return(datP)
      
    }
    
    
  }
  
  else if (input$use2 == TRUE) {
    
    startDateP <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S")
    
    endDateP <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S")
    
    datyP <- tryCatch({
      
      readNWISuv(siteNumber=input$yID, startDate = format(startDateP, "%Y-%m-%d"), 
                 endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datyP <- datyP[,c(1:5)]
    
    colnames(datyP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
    
    datxP <- tryCatch({
      
      readNWISuv(siteNumber = input$xID, startDate = format(startDateP, "%Y-%m-%d"),
                 endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datxP <- datxP[,c(1:6)]
    
    colnames(datxP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd", "timeZone")
    
    datx2P <- tryCatch({
      
      readNWISuv(siteNumber = input$xID2, startDate = format(startDateP, "%Y-%m-%d"),
                 endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datx2P <- datx2P[,c(1:5)]
    
    colnames(datx2P) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
    
    datyP$Flow <- ifelse(datyP$Flow == 0, NA, datyP$Flow)
    
    datxP$Flow <- ifelse(datxP$Flow == 0, NA, datxP$Flow)
    
    datx2P$Flow <- ifelse(datx2P$Flow == 0, NA, datx2P$Flow)
    
    colnames(datxP)[c(2,4:5)] <- paste0(colnames(datxP)[c(2,4:5)], ".x")
    
    datx2P <- datx2P[,-1]
    
    colnames(datx2P)[c(1,3:4)] <- paste0(colnames(datx2P)[c(1,3:4)], ".x2")
    
    datyP <- datyP[,-1]
    
    colnames(datyP)[c(1,3:4)] <- paste0(colnames(datyP)[c(1,3:4)], ".y")
    
    newDateP <- data.frame(dateTime = seq(from = as.POSIXct(startDateP), 
                                          to = as.POSIXct(endDateP), by = "15 min"))
    
    datxP <- merge(x = newDateP, y = datxP, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datx2P <- merge(x = newDateP, y = datx2P, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datyP <- merge(x = newDateP, y = datyP, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datx2P <- datx2P[,-c(1)]
    
    datyP <- datyP[,-c(1)]
    
    datxP <- datxP[,c(1,6,2:5)]
    
    datP <- cbind(datxP, datx2P, datyP)
    
    adjustLag1 <- input$lag1/15
    
    adjustLag2 <- input$lag2/15
    
    if (adjustLag1 == 0 & adjustLag2 == 0) {
      
      datP <- datP 
      
      return(datP)
      
    }
    
    else if (adjustLag1 == 0 & adjustLag2 > 0) {
      
      absLag2 <- abs(adjustLag2)
      
      datP$Flow.x2 <- dplyr::lead(datP$Flow.x2, absLag2)
      
      return(datP)
      
    }
    
    else if (ajdustLag1 == 0 & adjustLag2 < 0) {
      
      absLag2 <- abs(adjustLag2)
      
      datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
      
      return(datP)
      
    }
    
    else if (adjustLag1 > 0 & adjustLag2 == 0) {
      
      absLag1 <- abs(adjustLag1)
      
      datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag1)
      
      return(datP)
      
    }
    
    else if (ajdustLag1 < 0 & adjustLag2 == 0) {
      
      absLag1 <- abs(adjustLag1)
      
      regdatP$Flow.x <- dplyr::lag(datP$Flow.x, absLag1)
      
      return(datP)
      
    }
    
    else if (adjustLag1 > 0 & adjustLag2 > 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag2)
      
      datP$Flow.x2 <- dplyr::lead(datP$Flow.x2, absLag2)
      
      return(datP)
      
    }
    
    else if (ajdustLag1 < 0 & adjustLag2 > 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag2)
      
      datP$Flow.x2 <- dplyr::lead(datP$Flow.x2, absLag2)
      
      return(datP)
      
    }
    
    else if (adjustLag1 > 0 & adjustLag2 < 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag2)
      
      datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
      
      return(datP)
      
    }
    
    else if (ajdustLag1 < 0 & adjustLag2 < 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag2)
      
      datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
      
      return(datP)
      
    }
    
  }
  
}