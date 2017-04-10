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