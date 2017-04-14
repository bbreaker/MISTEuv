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
                              menuItem("Data Estimation", tabName = "tabset2",
                                       checkboxInput("log10", 
                                                     "Plot y axis in log10 scale for time series plots", 
                                                     value = TRUE),
                                       textInput("regDateSt", 
                                                 "begin date/time for model", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("regDateEn", 
                                                 "end date/time for model", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("estDateSt", 
                                                 "begin date/time for estimation", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("estDateEn", 
                                                 "end date/time for estimation", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("yID", 
                                                 "Station ID for response station", "yID"),
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
                                       textInput("smthDateSt",
                                                 "begin date and time for smoothing",
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("smthDateEn", 
                                                 "end date and time for smoothing", 
                                                 value = "9999-99-99 99:99:99"),
                                       checkboxInput("smooth", 
                                                     "Apply smoothing to estimated data", 
                                                     value = FALSE),
                                       radioButtons("Method", label = "Select regression method", 
                                                    choices = list("Linear" = 1, "GAM" = 2), selected = 2)
                              ),
                              menuItem("Data Estimation (Event)", tabName = "tabset4",
                                       checkboxInput("eventEst",
                                                     "Click if the estimation is for an event",
                                                     value = FALSE),
                                       checkboxInput("givePeakQ",
                                                     "Use an observed peak discharge", 
                                                     value = FALSE),
                                       textInput("peakToUse", 
                                                 "Observed peak discharge for response station",
                                                 value = "NULL"),
                                       textInput("peakDate",
                                                 "date and time peak occurred",
                                                 value = "9999-99-99 99:99:99")
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
      
      startDateP <- as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      endDateP <- as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
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
      
      else if (ajdustLag1 < 0 & adjustLag2 > 0) {
        
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
      
      else if (ajdustLag1 < 0 & adjustLag2 < 0) {
        
        absLag1 <- abs(adjustLag1)
        
        absLag2 <- abs(adjustLag2)
        
        datP$Flow.x <- dplyr::lag(datP$Flow.x, absLag1)
        
        datP$Flow.x2 <- dplyr::lag(datP$Flow.x2, absLag2)
        
        return(datP)
        
      }
      
    }
    
  })
  
  applySmooth <- function(df) {
    
    startSm <- as.POSIXct(input$smthDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") - as.difftime(30, units = "mins")
    
    endSm <- as.POSIXct(input$smthDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "GMT") + as.difftime(30, units = "mins")
    
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
          
          datP <- cbind(estDat, datPred)
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
        else if (input$Method == 2) {
          
          regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
          
          predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x)
          
          estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)
          
          datPred <- data.frame(estVals)
          
          upr <- datPred$fit + (2.5 * datPred$se.fit)
          
          lwr <- datPred$fit - (2.5 * datPred$se.fit)
          
          upr <- regObj$family$linkinv(upr)
          
          lwr <- regObj$family$linkinv(lwr)
          
          datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3), 
                                fitUpper = signif(10^(upr), 3), 
                                fitLower = signif(10^(lwr), 3), 
                                standardError = signif(datPred$se.fit, 3))
          
          datP <- cbind(estDat, datPred)
          
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
          
          datP <- cbind(estDat, datPred)
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
        else if (input$Method == 2) {
          
          regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                          s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
          
          predSet <- data.frame(dateTime = estDat$dateTime, Flow.x = estDat$Flow.x, Flow.x2 = estDat$Flow.x2)
          
          estVals <- predict(regObj, predSet, type = "link", se.fit = TRUE)
          
          datPred <- data.frame(estVals)
          
          upr <- datPred$fit + (2.5 * datPred$se.fit)
          
          lwr <- datPred$fit - (2.5 * datPred$se.fit)
          
          upr <- regObj$family$linkinv(upr)
          
          lwr <- regObj$family$linkinv(lwr)
          
          datPred <- data.frame(Estimated = signif(10^(datPred$fit), 3), 
                                fitUpper = signif(10^(upr), 3), 
                                fitLower = signif(10^(lwr), 3), 
                                standardError = signif(datPred$se.fit, 3))
          
          datP <- cbind(estDat, datPred)
          
          if (input$smooth == TRUE) {
            
            datP <- applySmooth(df = datP)
            
          }
          
          return(datP)
          
        }
        
      }
      
    }
    
    else if(input$eventEst == TRUE) {
      
      maxQ <- max(regDat$Flow.y, na.rm = TRUE)
      
      maxQdt <- regDat[regDat$Flow.y == maxQ,]
      
      if(nrow(maxQdt) > 1) {
        
        maxQdt <- maxQdt[!is.na(maxQdt$Flow_cd.y),]
        
        maxQdt <- maxQdt[round(nrow(maxQdt)/2, 0),1]
        
      }
      
      else if(nrow(maxQdt) == 1) {
        
        maxQdt <- maxQdt[1,1]
        
      }
      
      regDatRise <- regDat[regDat$dateTime <= maxQdt,]
      
      regDatFall <- regDat[regDat$dateTime >= maxQdt,]
      
      if(input$givePeakQ == FALSE) {
        
        maxQP <- max(estDat$Flow.y, na.rm = TRUE)
        
        maxQPdt <- estDat[estDat$Flow.y == maxQP,]
        
        if(nrow(maxQPdt) > 1) {
          
          maxQPdt <- maxQPdt[!is.na(maxQPdt$Flow.y),]
          
          maxQPdt <- maxQPdt[round(nrow(maxQPdt)/2, 0),1]
          
        }
        
        else if(nrow(maxQPdt) == 1) {
          
          maxQPdt <- maxQPdt[1,1]
          
        }
        
        estDatRise <- estDat[estDat$dateTime <= maxQPdt,]
        
        estDatFall <- estDat[estDat$dateTime >= maxQPdt,]
        
      }
      
      else if(input$givePeakQ == TRUE) {
        
        maxQP <- as.numeric(input$peakToUse)
        
        maxQPdt <- as.POSIXct(input$peakDate, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
        
        estDatRise <- estDat[estDat$dateTime <= maxQPdt,]
        
        estDatFall <- estDat[estDat$dateTime >= maxQPdt,]
        
        estDatRise[nrow(estDatRise),8] <- maxQP
        
        estDatFall[1,8] <- maxQP
        
      }
      
      regObjRise <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDatRise, select = TRUE)
      
      regObjFall <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDatFall, select = TRUE)
      
      predSetRise <- data.frame(dateTime = estDatRise$dateTime, Flow.x = estDatRise$Flow.x)
      
      predSetFall <- data.frame(dateTime = estDatFall$dateTime, Flow.x = estDatFall$Flow.x)
      
      estValsRise <- mgcv::predict.gam(regObjRise, predSetRise, type = "link", se.fit = TRUE)
      
      estValsFall <- mgcv::predict.gam(regObjFall, predSetFall, type = "link", se.fit = TRUE)
      
      datPredRise <- data.frame(estValsRise)
      
      uprRise <- datPredRise$fit + (2.5 * datPredRise$se.fit)
      
      lwrRise <- datPredRise$fit - (2.5 * datPredRise$se.fit)
      
      uprRise <- regObjRise$family$linkinv(uprRise)
      
      lwrRise <- regObjRise$family$linkinv(lwrRise)
      
      datPredRise <- data.frame(Estimated = 10^(datPredRise$fit), fitUpper = 10^(uprRise), 
                                fitLower = 10^(lwrRise), standardError = datPredRise$se.fit)
      
      datPredFall <- data.frame(estValsFall)
      
      uprFall <- datPredFall$fit + (2.5 * datPredFall$se.fit)
      
      lwrFall <- datPredFall$fit - (2.5 * datPredFall$se.fit)
      
      uprFall <- regObjFall$family$linkinv(uprFall)
      
      lwrFall <- regObjFall$family$linkinv(lwrFall)
      
      datPredFall <- data.frame(Estimated = 10^(datPredFall$fit), fitUpper = 10^(uprFall), 
                                fitLower = 10^(lwrFall), standardError = datPredFall$se.fit)
      
      datPredFall <- datPredFall[-1,]
      
      datPred <- dplyr::bind_rows(datPredRise, datPredFall)
      
      datPred <- data.frame(Estimated = signif(datPred$Estimated, 3), 
                            fitUpper = signif(datPred$fitUpper, 3),
                            fitLower = signif(datPred$fitLower, 3), 
                            standardError = signif(datPred$standardError, 3))
      
      datP <- dplyr::bind_cols(estDat, datPred)
      
      if (input$smooth == TRUE) {
        
        datP <- applySmooth(df = datP)
        
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