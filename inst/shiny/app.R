library(MISTEuv)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(dataRetrieval)
library(zoo)
library(lubridate)
library(gam)
library(mgcv)

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
                                                 "begin date and time for regression", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("regDateEn", 
                                                 "end date and time for regression", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("estDateSt", 
                                                 "begin date and time for estimation", 
                                                 value = "9999-99-99 99:99:99"),
                                       textInput("estDateEn", 
                                                 "end date and time for estimation", 
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
                              downloadButton("downloadData", "Download")
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
                    plotOutput("plotComp2", height = 400)))
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
  
  getEstDat <- reactive({
    
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
    
  })
  
  getRegDat <- reactive({
    
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
    
  })
  
  ##################################################################
  
  source("tableAllDat.R",local=TRUE)$value
  
  ##################################################################
  
  #################################################################
  
  source("plotAllDat.R",local=TRUE)$value
  
  ##################################################################
  
  #################################################################
  
  source("plotComp1.R",local=TRUE)$value
  
  ##################################################################
  
  #################################################################
  
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
  
})

shinyApp(ui, server)