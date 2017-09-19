library(MISTEuv)
library(shiny)
library(shinydashboard)
library(DT)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(dataRetrieval)
library(zoo)
library(lubridate)
library(mgcv)
library(broom)
library(hydroGOF)

source("global.R")

options(DT.options = list(pageLength = 7, language = list(search = 'Filter:')))

header <- dashboardHeader(title = "MISTEuv", titleWidth = 300,
                          tags$li(a(href = "https://www.usgs.gov/products/data-and-tools/real-time-data/water",
                                    img(src = "gsLogoBlack.png",
                                        title = "USGS home page", height = "47px"),
                                    style = "padding-top:3px; padding-bottom:0px;"),
                                  class = "dropdown"))

sidebar <- dashboardSidebar(width = 300, 
                            sidebarMenu(
                              submitButton("Apply changes", icon("paper-plane")),
                              menuItem("Data Estimation", tabName = "tabset2",
                                       checkboxInput("log10", 
                                                     "Plot y axis in log10 scale for time series plots", 
                                                     value = TRUE),
                                       textInput("yID", 
                                                 "Station ID for response station", "yID"),
                                       textInput("regDateSt", 
                                                 "begin date/time for model (UTC)", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("regDateEn", 
                                                 "end date/time for model (UTC)", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("estDateSt", 
                                                 "begin date/time for estimation (UTC)", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("estDateEn", 
                                                 "end date/time for estimation (UTC)", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("xID", 
                                                 "Station ID for explanatory station 1", "xID"),
                                       sliderInput("lag1", 
                                                   "Lag for explanatory station 1 (in minutes)", 
                                                   step = 15, min = -1440, max = 1440, value = 0),
                                       checkboxInput("use2", 
                                                     "Use a second explanatory station", 
                                                     value = FALSE),
                                       textInput("xID2", 
                                                 "Station ID for explanatory station 2", "xID2"),
                                       sliderInput("lag2", 
                                                   "Lag for explanatory station 2 (in minutes)", 
                                                   step = 15, min = -1440, max = 1440, value = 0)
                              ),
                              menuItem("Regression Tuning", tabName = "tabset3",
                                       checkboxInput("smooth", 
                                                     "Apply smoothing to estimated data", 
                                                     value = FALSE),
                                       radioButtons("Method", 
                                                    label = "Select regression method", 
                                                    choices = list("Linear" = 1, "GAM" = 2), 
                                                    selected = 2),
                                       checkboxInput("adjKnots", 
                                                     "Adjust GAM Knots", 
                                                     value = FALSE),
                                       numericInput("knots", 
                                                    "Number of knots to use", 
                                                    value = 3, min = 1, max = 10, step = 1),
                                       selectInput("roundOut", 
                                                   "Select hours to average by",
                                                   choices = list("none" = 0, "1 hour" = 1, "2 hours" = 2,
                                                                  "4 hours" = 4, "8 hours" = 8, "24 hours" = 24),
                                                   selected = 0),
                                       checkboxInput("giveObsQ",
                                                     "Add observed discharge", 
                                                     value = FALSE),
                                       textInput("ObsQ",
                                                 "Observed discharge",
                                                 value = "NA"),
                                       textInput("ObsQDate",
                                                 "Dates/times observations occurred (UTC)",
                                                 value = "9999-99-99 99:99:99")
                              ),
                              menuItem("Data Estimation (Event)", tabName = "tabset4",
                                       checkboxInput("eventEst",
                                                     "Click if the estimation is for an event",
                                                     value = FALSE),
                                       checkboxInput("givePeakQ",
                                                     "Specify peak discharge", 
                                                     value = FALSE),
                                       textInput("peakToUse", 
                                                 "Observed/estimated peak for response station", 
                                                 value = "NA")
                              ),
                                  downloadButton("downloadData", "Download Data"),
                                  downloadButton("downloadSum", "Download Summary")
                            )
)

body <- dashboardBody(
  fluidRow(
    tabBox(id = "tabset1", height = "450px", width = "900px",
           tabPanel("Time Series Est",
                    plotOutput("plotAllDat", height = 400)),
           tabPanel("Data Table", 
                    DT::dataTableOutput ("tableAllDat")),
           tabPanel("Time Series Comp 1",
                    plotOutput("plotComp1", height = 400)),
           tabPanel("Times Series Comp 2",
                    plotOutput("plotComp2", height = 400)),
           tabPanel("Residuals",
                    plotOutput("plotResids", height = 400)))
  ),
  fluidRow(
    tabBox(id = "tabset2", side ="left", height = "450px",
           tabPanel("Estimated vs Observed",
                    plotOutput("plotEvsO", height = 400, width = 400)),
           tabPanel("Response vs Explanatory 1",
                    plotOutput("plotXY1", height = 400, width = 400)),
           tabPanel("Response vs Explanatory 2",
                    plotOutput("plotXY2", height = 400, width = 400)),
           tabPanel("Smoothed UVs",
                    plotOutput("plotSmth", height = 400))
    ),
    tabBox(id = "tabset3", side = "left", height = "465px",
           tabPanel("Regression Summary",
                    verbatimTextOutput("regSum")),
           tabPanel("Regression Description",
                    verbatimTextOutput("regCust"))
           
    )
  )
)

ui <- dashboardPage(skin = "black", header, sidebar, body)

server <- function(input, output) ({
  
  options(shiny.sanitize.errors = TRUE)
  
  getEstDat <- reactive({
    
    if (input$use2 == FALSE) {
      
      startDateP <- as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDateP <- as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      datyP <- tryCatch({
        
        readNWISuv(siteNumber = input$yID, startDate = format(startDateP, "%Y-%m-%d"), 
                   endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
      },
      
      error = function(cond) {
        
        message("please enter a valid USGS station ID and date range")
        
      })
      
      datyP <- datyP[,c(1:5)]
      
      colnames(datyP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
      
      datxP <- tryCatch({
        
        readNWISuv(siteNumber = input$xID, startDate = format(startDateP, "%Y-%m-%d"),
                   endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
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
      
      startDateP <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDateP <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      datyP <- tryCatch({
        
        readNWISuv(siteNumber=input$yID, startDate = format(startDateP, "%Y-%m-%d"), 
                   endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
      },
      
      error = function(cond) {
        
        message("please enter a valid USGS station ID and date range")
        
      })
      
      datyP <- datyP[,c(1:5)]
      
      colnames(datyP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
      
      datxP <- tryCatch({
        
        readNWISuv(siteNumber = input$xID, startDate = format(startDateP, "%Y-%m-%d"),
                   endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
      },
      
      error = function(cond) {
        
        message("please enter a valid USGS station ID and date range")
        
      })
      
      datxP <- datxP[,c(1:6)]
      
      colnames(datxP) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd", "timeZone")
      
      datx2P <- tryCatch({
        
        readNWISuv(siteNumber = input$xID2, startDate = format(startDateP, "%Y-%m-%d"),
                   endDate = format(endDateP, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
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
      
      else if (adjustLag1 == 0 & adjustLag2 < 0) {
        
        absLag2 <- abs(adjustLag2)
        
        datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
        
        return(datP)
        
      }
      
      else if (adjustLag1 > 0 & adjustLag2 == 0) {
        
        absLag1 <- abs(adjustLag1)
        
        datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag1)
        
        return(datP)
        
      }
      
      else if (adjustLag1 < 0 & adjustLag2 == 0) {
        
        absLag1 <- abs(adjustLag1)
        
        datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag1)
        
        return(datP)
        
      }
      
      else if (adjustLag1 > 0 & adjustLag2 > 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag1)
        
        datP$Flow.x2 <- dplyr::lead(datP$Flow.x2, absLag2)
        
        return(datP)
        
      }
      
      else if (adjustLag1 < 0 & adjustLag2 > 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag1)
        
        datP$Flow.x2 <- dplyr::lead(datP$Flow.x2, absLag2)
        
        return(datP)
        
      }
      
      else if (adjustLag1 > 0 & adjustLag2 < 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        datP$Flow.x <- dplyr::lead(datP$Flow.x, absLag1)
        
        datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
        
        return(datP)
        
      }
      
      else if (adjustLag1 < 0 & adjustLag2 < 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag1)
        
        datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
        
        return(datP)
        
      }
      
    }
    
  })
  
  applySmooth <- function(df) {
    
    startSm <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") - as.difftime(30, units = "mins")
    
    endSm <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") + as.difftime(30, units = "mins")
    
    smPeriod <- dplyr::filter(df, dateTime >= startSm & dateTime <= endSm)
    
    allResids <- smPeriod$Flow.y - smPeriod$Estimated
    
    leftResid <- (allResids[1] + allResids[2]) / 2
    
    rightResid <- ((allResids[(length(allResids))]) + (allResids[(length(allResids) - 1)])) / 2
    
    diffDates <- as.numeric(endSm) - as.numeric(startSm)
    
    slopeResid <- (rightResid - leftResid) / diffDates
    
    intercept <- leftResid
    
    smPeriod$adjResid <- intercept + slopeResid*(as.numeric(smPeriod$dateTime) - as.numeric(startSm))
    
    smPeriod$Smoothed <- smPeriod$Estimated + smPeriod$adjResid
    
    smPeriod <- data.frame(dateTime = as.POSIXct(smPeriod$dateTime, format = "%Y-%m-%d %H:%M:%S"), 
                           Smoothed = signif(smPeriod$Smoothed, 3), 
                           adjResid = signif(smPeriod$adjResid, 3))
    
    datP <- merge(x = df, y = smPeriod, by = "dateTime", all.x = TRUE)
    
    return(datP)
    
  }
  
  getRegDat <- reactive({
    
    if (input$use2 == FALSE) {
      
      startDate <- as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDate <- as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      daty <- tryCatch({
        
        readNWISuv(siteNumber = input$yID, startDate = format(startDate, "%Y-%m-%d"), 
                   endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
      },
      
      error = function(cond){
        
        message("please enter a valid USGS station ID and date range")
        
      })
      
      daty <- daty[,c(1:5)]
      
      colnames(daty) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
      
      datx <- tryCatch({
        
        readNWISuv(siteNumber = input$xID, startDate = format(startDate, "%Y-%m-%d"),
                   endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
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
      
      newDateReg <- data.frame(dateTime = seq(from = as.POSIXct(startDate), 
                                           to = as.POSIXct(endDate), by = "15 min"))
      
      startDateNa <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDateNa <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      newDateNa <- data.frame(dateTime = seq(from = as.POSIXct(startDateNa),
                                             to = as.POSIXct(endDateNa), by = "15 min"))
      
      datx <- merge(x = newDateReg, y = datx, by = "dateTime", all.x = TRUE, all.y = FALSE)
      
      daty <- merge(x = newDateReg, y = daty, by = "dateTime", all.x = TRUE, all.y = FALSE)
      
      check <- match(newDateNa$dateTime, daty$dateTime)
      
      daty[check[1]:check[length(check)],3] <- NA 
      
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
      
      startDate <- as.POSIXct(input$regDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDate <- as.POSIXct(input$regDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      daty <- tryCatch({
        
        readNWISuv(siteNumber=input$yID, startDate = format(startDate, "%Y-%m-%d"), 
                   endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
      },
      
      error = function(cond) {
        
        message("please enter a valid USGS station ID and date range")
        
      })
      
      daty <- daty[,c(1:5)]
      
      colnames(daty) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd")
      
      datx <- tryCatch({
        
        readNWISuv(siteNumber=input$xID, startDate = format(startDate, "%Y-%m-%d"),
                   endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
      },
      
      error = function(cond) {
        
        message("please enter a valid USGS station ID and date range")
        
      })
      
      datx <- datx[,c(1:6)]
      
      colnames(datx) <- c("agency_cd", "site_no", "dateTime", "Flow", "Flow_cd", "timeZone")
      
      datx2 <- tryCatch({
        
        readNWISuv(siteNumber=input$xID2, startDate = format(startDate, "%Y-%m-%d"),
                   endDate = format(endDate, "%Y-%m-%d"), parameterCd = "00060",
                   tz = "")
        
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
      
      newDateReg <- data.frame(dateTime = seq(from = as.POSIXct(startDate), 
                                           to = as.POSIXct(endDate), by = "15 min"))
      
      startDateP <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDateP <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      newDateP <- data.frame(dateTime = seq(from = as.POSIXct(startDateP), 
                                            to = as.POSIXct(endDateP), by = "15 min"))
      
      datx <- merge(x = newDateReg, y = datx, by = "dateTime", all.x = TRUE, all.y = FALSE)
      
      datx2 <- merge(x = newDateReg, y = datx2, by = "dateTime", all.x = TRUE, all.y = FALSE)
      
      daty <- merge(x = newDateReg, y = daty, by = "dateTime", all.x = TRUE, all.y = FALSE)
      
      check <- match(newDateP$dateTime, daty$dateTime)
      
      daty[check[1]:check[length(check)],3] <- NA
      
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
        
        dat$Flow.x <- dplyr::lag(dat$Flow.x, absLag1)
        
        return(dat)
        
      }
      
      else if (adjustLag1 > 0 & adjustLag2 > 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        dat$Flow.x <- dplyr::lead(dat$Flow.x, absLag1)
        
        dat$Flow.x2 <- dplyr::lead(dat$Flow.x2, absLag2)
        
        return(dat)
        
      }
      
      else if (adjustLag1 < 0 & adjustLag2 > 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        dat$Flow.x <- dplyr::lag(dat$Flow.x, absLag1)
        
        dat$Flow.x2 <- dplyr::lead(dat$Flow.x2, absLag2)
        
        return(dat)
        
      }
      
      else if (adjustLag1 > 0 & adjustLag2 < 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        dat$Flow.x <- dplyr::lead(dat$Flow.x, absLag1)
        
        dat$Flow.x2 <- dplyr::lag(dat$Flow.x2, absLag2)
        
        return(dat)
        
      }
      
      else if (adjustLag1 < 0 & adjustLag2 < 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        dat$Flow.x <- dplyr::lag(dat$Flow.x, absLag1)
        
        dat$Flow.x2 <- dplyr::lag(dat$Flow.x2, absLag2)
        
        return(dat)
        
      }
      
    }
    
  })
  
  allMISTEdat <- function(regDat, estDat) {
    
    if(input$eventEst == FALSE) {
      
      if (input$use2 == FALSE) {
        
        if (input$Method == 1) {
          
          regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
          
          predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x)
          
          estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")
          
          datPred <- data.frame(estVals)
          
          datPred[,(1:4)] <- 10^datPred[,(1:4)]
          
          datPred <- data.frame(Estimated = signif(datPred$fit.fit, 3), 
                                fitUpper = signif(datPred$fit.upr, 3), 
                                fitLower = signif(datPred$fit.lwr, 3), 
                                standardError = signif(datPred$se.fit, 3))
          
          duansSm <- (sum(10^(resid(regObj))))/nobs(regObj)
          
          datPred$Estimated <- signif(datPred$Estimated*duansSm, 3)
          
          datP <- cbind(regDat, datPred)
          
          if (input$roundOut != 0) {
            
            datAve <- aveEstData(df = datP, n = as.numeric(input$roundOut))
            
            datP <- datP[,-c(10:13)]
            
            datP <- dplyr::left_join(x = datP, y = datAve, by = "dateTime")
            
          }
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
        else if (input$Method == 2) {
          
          if(input$adjKnots == FALSE) {
            
            regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cs"), data = regDat, select = TRUE)
            
          }
          
          else if (input$adjKnots == TRUE) {
            
            regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cr", k = input$knots), data = regDat)
            
          }
          
          predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x)
          
          estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)
          
          datPred <- data.frame(estVals)
          
          upr <- gamIntervals(estVals, regObj, interval = "prediction")$upr
          
          lwr <- gamIntervals(estVals, regObj, interval = "prediction")$lwr
          
          datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3), 
                                fitUpper = signif(10^(upr), 3), 
                                fitLower = signif(10^(lwr), 3), 
                                standardError = signif(datPred$se.fit, 3))
          
          duansSm <- (sum(10^(resid(regObj))))/nobs(regObj)
          
          datPred$Estimated <- signif(datPred$Estimated*duansSm, 3)
          
          datP <- cbind(regDat, datPred)
          
          if (input$roundOut != 0) {
            
            datAve <- aveEstData(df = datP, n = as.numeric(input$roundOut))
            
            datP <- datP[,-c(10:13)]
            
            datP <- dplyr::left_join(x = datP, y = datAve, by = "dateTime")
            
          }
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
      }
      
      else if (input$use2 == TRUE) {
        
        if (input$Method == 1) {
          
          regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
          
          predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x, Flow.x2 = estDat$Flow.x2)
          
          estVals <- predict(regObj, predSet, se.fit = TRUE, interval = "prediction")
          
          datPred <- data.frame(estVals)
          
          datPred[,(1:4)] <- 10^datPred[,(1:4)]
          
          datPred <- data.frame(Estimated = signif(datPred$fit.fit, 3), 
                                fitUpper = signif(datPred$fit.upr, 3), 
                                fitLower = signif(datPred$fit.lwr, 3), 
                                standardError = signif(datPred$se.fit, 3))
          
          duansSm <- (sum(10^(resid(regObj))))/nobs(regObj)
          
          datPred$Estimated <- signif(datPred$Estimated*duansSm, 3)
          
          datP <- cbind(regDat, datPred)
          
          if (input$roundOut != 0) {
            
            datAve <- aveEstData(df = datP, n = as.numeric(input$roundOut))
            
            datP <- datP[,-c(10:13)]
            
            datP <- dplyr::left_join(x = datP, y = datAve, by = "dateTime")
            
          }
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
        else if (input$Method == 2) {
          
          if (input$adjKnots == FALSE) {
            
            regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cs") + 
                            s(log10(Flow.x2), bs = "cs"), data = regDat)
            
          }
          
          else if (input$adjKnots == TRUE) {
            
            regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "cr", k = input$knots) + 
                            s(log10(Flow.x2), bs = "cr", k = input$knots), data = regDat)
            
          }
          
          predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x, Flow.x2 = estDat$Flow.x2)
          
          estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)
          
          datPred <- data.frame(estVals)
          
          upr <- gamIntervals(estVals, regObj, interval = "prediction")$upr
          
          lwr <- gamIntervals(estVals, regObj, interval = "prediction")$lwr
          
          datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3), 
                                fitUpper = signif(10^(upr), 3), 
                                fitLower = signif(10^(lwr), 3), 
                                standardError = signif(datPred$se.fit, 3))
          
          duansSm <- (sum(10^(resid(regObj))))/nobs(regObj)
          
          datPred$Estimated <- signif(datPred$Estimated*duansSm, 3)
          
          datP <- cbind(regDat, datPred)
          
          if (input$roundOut != 0) {
            
            datAve <- aveEstData(df = datP, n = as.numeric(input$roundOut))
            
            datP <- datP[,-c(10:13)]
            
            datP <- dplyr::left_join(x = datP, y = datAve, by = "dateTime")
            
          }
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
      }
      
    }
    
    else if(input$eventEst == TRUE) {
      
      regDat <- mutate(regDat, event = if_else(Flow.x > lag(Flow.x, 1), "rise", "fall"))
      
      regDat$event <- if_else(is.na(regDat$event), "rise", regDat$event)
      
      estDat <- mutate(estDat, event = if_else(Flow.x > lag(Flow.x, 1), "rise", "fall"))
      
      estDat$event <- if_else(is.na(estDat$event), "rise", estDat$event)
      
      if(input$adjKnots == FALSE) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), by = factor(event), bs = "fs", k = 10), 
                      data = regDat, select = TRUE)
        
      }
      
      else if(input$adjKnots == TRUE) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), by = factor(event), bs = "fs", k = input$knots), 
                      data = regDat, select = TRUE)
        
      }
      
      predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x, event = estDat$event)
      
      estVals <- mgcv::predict.gam(regObj, predSet, type = "link", se.fit = TRUE)
      
      datPred <- data.frame(dateTime = estDat$dateTime, estVals)
      
      uprInt <- gamIntervals(estVals, regObj, interval = "prediction")$upr
      
      lwrInt <- gamIntervals(estVals, regObj, interval = "prediction")$lwr
      
      datPred <- data.frame(dateTime = estDat$dateTime, Estimated = 10^(datPred$fit), fitUpper = 10^(uprInt), 
                            fitLower = 10^(lwrInt), standardError = datPred$se.fit)
      
      duansSm <- (sum(10^(resid(regObj))))/nobs(regObj)
      
      datPred$Estimated <- signif(datPred$Estimated*duansSm, 3)
      
      datPred <- data.frame(Estimated = signif(datPred$Estimated, 3), 
                            fitUpper = signif(datPred$fitUpper, 3),
                            fitLower = signif(datPred$fitLower, 3), 
                            standardError = signif(datPred$standardError, 3))
      
      datP <<- dplyr::bind_cols(x = regDat, y = datPred)
      
      if (input$roundOut != 0) {
        
        datAve <- aveEstData(df = datP, n = as.numeric(input$roundOut))
        
        datP <- datP[,-c(10:13)]
        
        datP <- dplyr::left_join(x = datP, y = datAve, by = "dateTime")
        
      }
      
      if (input$smooth == TRUE) {
        
        datP <- applySmooth(df = datP)
        
      }
      
      if(input$giveObsQ == TRUE) {
        
        addQs <- as.numeric(unlist(strsplit(input$ObsQ, ", ")))
        
        addDTs <- unlist(strsplit(input$ObsQDate, ", "))
        
        ObsDf <<- data.frame(dateTime = as.POSIXct(addDTs, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
                            Flow.obs = addQs)
        
        startDat <<- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        
        endDat <<- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        
        startSm <<- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") - as.difftime(30, units = "mins")
        
        endSm <<- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") + as.difftime(30, units = "mins")
        
        datP <- smoothAddQ(df = datP, ObsDf = ObsDf, startSm, endSm, startDat, endDat)
        
        datP <- dplyr::select(datP, -event)
        
      }
      
      if(input$givePeakQ == TRUE) {
        
        datP <- applySmooth(df = datP)
        
        addPeak <- as.numeric(input$peakToUse)
        
        datP$newEstimate <- if_else(datP$Estimated == max(datP$Estimated, na.rm = TRUE), addPeak, datP$Estimated)
        
        datP$Estimated <- if_else(is.na(datP$Flow_cd.y), datP$newEstimate, datP$Estimated)
        
        datP <- datP[, !names(datP) %in% "newEstimate"]
        
      }
      
      return(datP)
      
    }
    
  }
  
  ##################################################################
  
  source("tableAllDat.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotAllDat.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotComp1.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotComp2.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotEvsO.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotXY1.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotXY2.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("plotSmth.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("regSum.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("regCust.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("getDataFile.R",local=TRUE)$value
  
  ##################################################################
  
  ##################################################################
  
  source("getDataSummary.R",local=TRUE)$value
  
  ##################################################################

  ##################################################################
  
  source("plotResids.R",local=TRUE)$value
  
  ##################################################################
  
})

shinyApp(ui, server)