getRegDat <- function(session) {
  
  if (input$use2 == FALSE) {
    
    startDate <- as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S")
    
    endDate <- as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S")
    
    daty <- tryCatch({
      
      readNWISuv(siteNumber = input$yID, startDate = format(startDate, "%Y-%m-%d"), 
                 endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond){
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    daty <- daty[,c(1:5)]
    
    colnames(daty) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
    
    datx <- tryCatch({
      
      readNWISuv(siteNumber = input$xID, startDate = format(startDate, "%Y-%m-%d"),
                 endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond){
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datx <- datx[,c(1:6)]
    
    colnames(datx) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd", "timeZone")
    
    daty$Flow <- ifelse(daty$Flow == 0, NA, daty$Flow)
    
    datx$Flow <- ifelse(datx$Flow == 0, NA, datx$Flow)
    
    colnames(datx)[c(2,4:5)] <- paste0(colnames(datx)[c(2,4:5)], ".x")
    
    daty <- daty[,-1]
    
    colnames(daty)[c(1,3:4)] <- paste0(colnames(daty)[c(1,3:4)], ".y")
    
    newDate <- data.frame(dateTime = seq(from = as.POSIXct(startDate), 
                                         to = as.POSIXct(endDate), by = "15 min"))
    
    datx <- merge(x = newDate, y = datx, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    daty <- merge(x = newDate, y = daty, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    daty <- daty[,-c(1)]
    
    datx <- datx[,c(1,6,2:5)]
    
    dat <- cbind(datx, daty)
    
    adjustLag1 <- input$lag1/15
    
    if (adjustLag1 == 0) {
      
      return(dat)
      
    }
    
    else if (adjustLag1 > 0) {
      
      absLag1 <- abs(adjustLag1)
      
      dat$Flow.x <- dplyr::lead(dat$Flow.x, absLag1)
      
      return(dat)
      
    }
    
    else if (adjustLag1 < 0) {
      
      absLag1 <- abs(adjustLag1)
      
      dat$Flow.x <- dplyr::lag(dat$Flow.x, absLag1)
      
      return(dat)
      
    }
    
  }
  
  else if (input$use2 == TRUE) {
    
    startDate <- as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S")
    
    endDate <- as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S")
    
    daty <- tryCatch({
      
      readNWISuv(siteNumber=input$yID, startDate = format(startDate, "%Y-%m-%d"), 
                 endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    daty <- daty[,c(1:5)]
    
    colnames(daty) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
    
    datx <- tryCatch({
      
      readNWISuv(siteNumber=input$xID, startDate = format(startDate, "%Y-%m-%d"),
                 endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datx <- datx[,c(1:6)]
    
    colnames(datx) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd", "timeZone")
    
    datx2 <- tryCatch({
      
      readNWISuv(siteNumber=input$xID2, startDate = format(startDate, "%Y-%m-%d"),
                 endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                 tz = Sys.timezone())
      
    },
    
    error = function(cond) {
      
      message("please enter a valid USGS station ID and date range")
      
    })
    
    datx2 <- datx2[,c(1:5)]
    
    colnames(datx2) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
    
    daty$Flow <- ifelse(daty$Flow == 0, NA, daty$Flow)
    
    datx$Flow <- ifelse(datx$Flow == 0, NA, datx$Flow)
    
    datx2$Flow <- ifelse(datx2$Flow == 0, NA, datx2$Flow)
    
    colnames(datx)[c(2,4:5)] <- paste0(colnames(datx)[c(2,4:5)], ".x")
    
    datx2 <- datx2[,-1]
    
    colnames(datx2)[c(1,3:4)] <- paste0(colnames(datx2)[c(1,3:4)], ".x2")
    
    daty <- daty[,-1]
    
    colnames(daty)[c(1,3:4)] <- paste0(colnames(daty)[c(1,3:4)], ".y")
    
    newDate <- data.frame(dateTime = seq(from = as.POSIXct(startDate), 
                                         to = as.POSIXct(endDate), by = "15 min"))
    
    datx <- merge(x = newDate, y = datx, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datx2 <- merge(x = newDate, y = datx2, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    daty <- merge(x = newDate, y = daty, by = "dateTime", all.x = TRUE, all.y = FALSE)
    
    datx2 <- datx2[,-c(1)]
    
    daty <- daty[,-c(1)]
    
    datx <- datx[,c(1,6,2:5)]
    
    dat <- cbind(datx, datx2, daty)
    
    adjustLag1 <- input$lag1/15
    
    adjustLag2 <- input$lag2/15
    
    if (adjustLag1 == 0 & adjustLag2 == 0) {
      
      dat <- dat 
      
      return(dat)
      
    }
    
    else if (adjustLag1 == 0 & adjustLag2 > 0) {
      
      absLag2 <- abs(adjustLag2)
      
      dat$Flow.x2 <- dplyr::lead(dat$Flow.x2, absLag2)
      
      return(dat)
      
    }
    
    else if (adjustLag1 == 0 & adjustLag2 < 0) {
      
      absLag2 <- abs(adjustLag2)
      
      dat$Flow.x2 <- dplyr::lag(dat$Flow.x2, absLag2)
      
      return(dat)
      
    }
    
    else if (adjustLag1 > 0 & adjustLag2 == 0) {
      
      absLag1 <- abs(adjustLag1)
      
      dat$Flow.x <- dplyr::lead(dat$Flow.x, absLag1)
      
      return(dat)
      
    }
    
    else if (adjustLag1 < 0 & adjustLag2 == 0) {
      
      absLag1 <- abs(adjustLag1)
      
      regDat$Flow.x <- dplyr::lag(dat$Flow.x, absLag1)
      
      return(dat)
      
    }
    
    else if (adjustLag1 > 0 & adjustLag2 > 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      dat$Flow.x <- dplyr::lead(dat$Flow.x, absLag2)
      
      dat$Flow.x2 <- dplyr::lead(dat$Flow.x2, absLag2)
      
      return(dat)
      
    }
    
    else if (adjustLag1 < 0 & adjustLag2 > 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      dat$Flow.x <- dplyr::lag(dat$Flow.x, absLag2)
      
      dat$Flow.x2 <- dplyr::lead(dat$Flow.x2, absLag2)
      
      return(dat)
      
    }
    
    else if (adjustLag1 > 0 & adjustLag2 < 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      dat$Flow.x <- dplyr::lead(dat$Flow.x, absLag2)
      
      dat$Flow.x2 <- dplyr::lag(dat$Flow.x2, absLag2)
      
      return(dat)
      
    }
    
    else if (adjustLag1 < 0 & adjustLag2 < 0) {
      
      absLag1 <- abs(adjustLag1)
      
      absLag2 <- abs(adjustLag2)
      
      dat$Flow.x <- dplyr::lag(dat$Flow.x, absLag2)
      
      dat$Flow.x2 <- dplyr::lag(dat$Flow.x2, absLag2)
      
      return(dat)
      
    }
    
  }
  
}