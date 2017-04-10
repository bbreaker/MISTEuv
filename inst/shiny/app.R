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

server <- function(input, output, session) ({
  
  source("global.R")
  
  options(shiny.sanitize.errors = TRUE)
  
  regDat <- reactive({getRegDat()})
  
  estDat <- reactive({getEstDat()})
  
  datP <- reactive({allMISTEdat()})
  
  output$tableAllDat <- DT::renderDataTable({
    
    DT::datatable(datP, options = list(scrollX = TRUE, scrolly = TRUE), rownames = FALSE)
    
  })
  
  output$plotAllDat <- renderPlot({
    
    if(input$eventEst == FALSE) {
      
      if (input$use2 == FALSE) {
        
        if (input$log10 == FALSE) {
          
          p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())
          
        }	
        
        else if (input$log10 == TRUE) {
          
          p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
          
        }
        
        p
        
      }
      
      else if (input$use2 == TRUE) {
        
        if (input$log10 == FALSE) {
          
          p <- ggplot(data = datP) +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
            geom_line(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())
          
        }
        
        else if (input$log10 == TRUE) {
          
          p <- ggplot(data = datP) +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
            geom_line(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
          
        }
        
        p
        
      }
      
    }
    
    else if(input$eventEst == TRUE) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line() +
          geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
          geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
          geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
          scale_y_continuous(label = comma) +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line() +
          geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
          geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
          geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
          scale_y_log10(label = comma) +
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  })
  
  output$plotComp1 <- renderPlot({
    
    bestOffsetVal <- getBestOffset(datP) 
    
    if(input$lag1 == 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"), 
                   y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], format = "%Y-%m-%d %H:%M:%S")) + 
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          scale_y_log10() +
          annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"),
                   y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], format = "%Y-%m-%d %H:%M:%S")) + 
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
    else if(input$lag1 != 0) {
      
      if (input$log10 == FALSE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank())
        
      }
      
      else if (input$log10 == TRUE) {
        
        p <- ggplot() +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
          geom_line(data = regDat, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID)), linetype = "dotdash") +
          scale_y_log10() +
          annotation_logticks(sides = "rl") +
          labs(x = "Date", y = "Discharge, in cubic feet per second") +
          theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
        
      }
      
      p
      
    }
    
  })
  
  output$plotComp2 <- renderPlot({
    
    if (input$use2 == TRUE) {
      
      bestOffsetVal <- getBestOffset(datP) 
      
      if(input$lag2 == 0) {
        
        if (input$log10 == FALSE) {
          
          p <- ggplot() +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
            annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"), 
                     y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], format = "%Y-%m-%d %H:%M:%S")) +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())
          
        }
        
        else if (input$log10 == TRUE) {
          
          p <- ggplot() +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y2-", input$yID))) +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
            scale_y_log10() +
            annotate("text", label = paste0("calculated best time offset = ", bestOffsetVal, " minutes"), 
                     y = 0.99*(max(regDat$Flow.x)), x = as.POSIXct(regDat[nrow(regDat)/2,1], format = "%Y-%m-%d %H:%M:%S")) +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
          
        }
        
        p
        
      }
      
      else if(input$lag2 != 0) {
        
        if (input$log10 == FALSE) {
          
          p <- ggplot() +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())
          
        }
        
        else if (input$log10 == TRUE) {
          
          p <- ggplot() +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.y, color = paste0("y2-", input$yID))) +
            geom_line(data = regDat, aes(x = dateTime, y = Flow.x2, color = paste0("x2-", input$xID2)), linetype = "dotdash") +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
          
        }
        
        p
        
      }
      
    }
    
  })
  
  output$plotEvsO <- renderPlot({
    
    p <- ggplot(data = datP, aes(x = Flow.y, y = Estimated)) +
      geom_point(size = 3) +
      stat_smooth(method = "lm") + 
      labs(x = "Observed discharge, in cubic feet per second", 
           y = "Estimated discharge, in cubic feet per second") +
      theme_bw() + theme(legend.title = element_blank())
    
    p
    
  })
  
  output$plotXY1 <- renderPlot({
    
    if (input$Method == 1) {
      
      p <- ggplot(data = datP, aes(x = Flow.x, y = Flow.y)) +
        geom_point(size = 3) +
        scale_y_log10() +
        scale_x_log10() +
        annotation_logticks(sides = "trbl") +
        stat_smooth(method = "lm") + 
        labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID), 
             y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
        theme_bw() + theme(legend.title = element_blank())
      
      p
      
    }
    
    else if (input$Method == 2) {
      
      p <- ggplot(data = datP, aes(x = Flow.x, y = Flow.y)) +
        geom_point(size = 3) +
        scale_y_log10() +
        scale_x_log10() +
        annotation_logticks(sides = "trbl") +
        stat_smooth(method = "gam", formula = y ~ s(x, bs = "ts")) + 
        labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID), 
             y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
        theme_bw() + theme(legend.title = element_blank())
      
      p
      
    }
    
  })
  
  output$plotXY2 <- renderPlot({
    
    if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        p <- ggplot(data = datP, aes(x = Flow.x2, y = Flow.y)) +
          geom_point() +
          scale_y_log10() +
          scale_x_log10() +
          annotation_logticks(sides = "trbl") +
          stat_smooth(method = "lm") +
          labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2), 
               y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
          theme_bw() + theme(legend.title = element_blank())
        
        p
        
      }
      
      else if (input$Method == 2) {
        
        p <- ggplot(data = datP, aes(x = Flow.x2, y = Flow.y)) +
          geom_point() +
          scale_y_log10() +
          scale_x_log10() +
          annotation_logticks(sides = "trbl") +
          stat_smooth(method = "gam", formula = y ~ s(x)) +
          labs(x = paste0("Discharge, in cubic feet per second \n for ", input$xID2), 
               y = paste0("Discharge, in cubic feet per second \n for ", input$yID)) + 
          theme_bw() + theme(legend.title = element_blank())
        
        p
        
      }
      
    }
    
  })
  
  output$plotSmth <- renderPlot({
    
    if (input$smooth == TRUE) {
      
      if (input$use2 == FALSE) {
        
        if (input$log10 == FALSE) {
          
          p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
            #scale_y_log10() +
            #annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())
          
        }
        
        else if (inputlog10 == TRUE) {
          
          p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
          
        }
        
        p
        
      }
      
      else if (input$use2 == TRUE) {
        
        if (input$log10 == FALSE) {
          
          p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
            #scale_y_log10() +
            #annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank())
          
        }
        
        else if (input$log10 == TRUE) {
          
          p <- ggplot(data = datP, aes(x = dateTime, y = Flow.y, color = paste0("y-", input$yID))) +
            geom_line() +
            geom_line(data = datP, aes(x = dateTime, y = Flow.x, color = paste0("x-", input$xID))) +
            geom_line(data = datP, aes(x = dateTime, y = Estimated, color = "Estimated"), linetype = "longdash") +
            geom_line(data = datP, aes(x = dateTime, y = fitUpper), color = "grey", linetype = "dashed") +
            geom_line(data = datP, aes(x = dateTime, y = fitLower), color = "grey", linetype = "dashed") +
            geom_point(data = datP, aes(x = dateTime, y = Smoothed, color = "Smoothed"), shape = 8) +
            scale_y_log10() +
            annotation_logticks(sides = "rl") +
            labs(x = "Date", y = "Discharge, in cubic feet per second") +
            theme_bw() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())
          
        }
        
        p
        
      }
      
    }
    
  })
  
  output$regSum <- renderPrint({
    
    if(input$eventEst == FALSE) {
      
      if (input$use2 == FALSE) {
        
        if (input$Method == 1) {
          
          summary(lm(log10(Flow.y) ~ log10(Flow.x), data = regDat))
          
        }
        
        else if (input$Method == 2) {
          
          summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE))
          
        }
        
      }
      
      else if (input$use2 == TRUE) {
        
        if (input$Method == 1) {
          
          summary(lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat))
          
        }
        
        else if (input$Method == 2) {
          
          summary(gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + s(log10(Flow.x2), bs = "ts"), 
                      data = regDat, select = TRUE))
          
        }
        
      }
      
    }
    
  })
  
  output$regCust <- renderPrint({
    
    regDat <- getRegDat()
    
    estDat <- getEstDat()
    
    if (input$use2 == FALSE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)
        
        newSummary <- list()
        
        newSummary[[1]] <- paste0("Index site ", input$xID, " was used to estimate data for ", input$yID)
        
        newSummary
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)
        
        newSummary <- list()
        
        newSummary[[1]] <- paste0("Index site ", input$xID, " was used to estimate data for ", input$yID)
        
        newSummary
        
      }
      
    }
    
    else if (input$use2 == TRUE) {
      
      if (input$Method == 1) {
        
        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)
        
        newSummary <- list()
        
        newSummary[[1]] <- paste0("Index sites ", input$xID, " and ", input$xID2, " was used to estimate data for ", input$yID)
        
        newSummary
        
      }
      
      else if (input$Method == 2) {
        
        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") + 
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)
        
        newSummary <- list()
        
        newSummary[[1]] <- paste0("Index sites ", input$xID, " and ", input$xID2, " was used to estimate data for ", input$yID)
        
        newSummary
        
      }
      
    }
    
  })
  
})

shinyApp(ui, server)