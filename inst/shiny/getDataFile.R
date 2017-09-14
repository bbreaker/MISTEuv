output$downloadData <- downloadHandler(

  filename = function() {paste0("data", input$yID, ".mud")},

  content =  function(file) {

    estDat <- getEstDat()

    regDat <- getRegDat()

    datP <- allMISTEdat(estDat, regDat)

    if (input$smooth == FALSE) {

      datP <- datP %>%
        dplyr::filter(dateTime >= as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") |
                      dateTime <= as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
        dplyr::select(dateTime, Estimated)

      names(datP)[2] <- "Flow"

    }

    else if (input$smooth == TRUE) {

      datP <- datP %>%
        dplyr::filter(dateTime >= as.POSIXct(input$estDateSt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") |
                      dateTime <= as.POSIXct(input$estDateEn, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
        dplyr::select(dateTime, Smoothed)

      names(datP)[2] <- "Flow"

    }

    write.csv(datP, file, row.names = FALSE)

  }

)
