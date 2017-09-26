output$regCust <- renderPrint({

  estDat <- getEstDat()

  regDat <- getRegDat()

  datP <- allMISTEdat(estDat, regDat)

  newNames <- c("Estimates for", "Index 1", "Lag for Index 1", "Index 2", "Lag for Index 2", "Summary", "Method", "GAM Knots", "Adj R-squared",
                "Percent bias", "Transformation bias", "Regression range", "Estimation range", "Smoothing applied", "Smoothing date range",
                "Peak input", "Peak input date", "Observed Discharge", "Observed Discharge Date")

  if (input$eventEst == FALSE) {

    if (input$use2 == FALSE) {

      if (input$Method == 1) {

        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)

        newDF <- data.frame(sim = regObj$fitted.values, obs = regObj$model[1])

        testg <- gof(sim = newDF[,1], obs = newDF[,2])

        newSummary <- c(input$yID,
                        input$xID,
                        input$lag1,
                        "",
                        "",
                        "steady",
                        "linear regression",
                        "",
                        signif(summary(regObj)$adj.r.squared, 3),
                        testg[6,1],
                        round((sum(10^(resid(regObj))))/nobs(regObj),3),
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$regDateSt, " to ", input$regDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate),
                        "",
                        "")

      }

      else if (input$Method == 2) {

        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)

        testg <- gof(sim = regObj$fitted.values, obs = regObj$y)

        newSummary <- c(input$yID,
                        input$xID,
                        input$lag1,
                        "",
                        "",
                        "steady",
                        "GAM regression",
                        dplyr::if_else(input$adjKnots == FALSE, "Default", input$knots),
                        signif(summary(regObj)$r.sq, 3),
                        testg[6,1],
                        round((sum(10^(resid(regObj))))/nobs(regObj),3),
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$regDateSt, " to ", input$regDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate),
                        "",
                        "")

      }

    }

    else if (input$use2 == TRUE) {

      if (input$Method == 1) {

        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)

        newDF <- data.frame(sim = regObj$fitted.values, obs = regObj$model[1])

        testg <- gof(sim = newDF[,1], obs = newDF[,2])

        newSummary <- c(input$yID,
                        input$xID,
                        input$lag1,
                        input$xID2,
                        input$lag2,
                        "steady",
                        "linear regression",
                        "",
                        signif(summary(regObj)$adj.r.squared, 3),
                        testg[6,1],
                        round((sum(10^(resid(regObj))))/nobs(regObj),3),
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$regDateSt, " to ", input$regDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate),
                        "",
                        "")

      }

      else if (input$Method == 2) {

        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") +
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)

        testg <- gof(sim = regObj$fitted.values, obs = regObj$y)

        newSummary <- c(input$yID,
                        input$xID,
                        input$lag1,
                        input$xID2,
                        input$lag2,
                        "steady",
                        "GAM regression",
                        dplyr::if_else(input$adjKnots == FALSE, "Default", input$knots),
                        signif(summary(regObj)$r.sq, 3),
                        testg[6,1],
                        round((sum(10^(resid(regObj))))/nobs(regObj),3),
                        paste0(input$regDateSt, " to ", input$regDateEn),
                        paste0(input$estDateSt, " to ", input$estDateEn),
                        dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                        dplyr::if_else(input$smooth == FALSE, "", paste0(input$regDateSt, " to ", input$regDateEn)),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                        dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate),
                        "",
                        "")

      }

    }

  }

  else if (input$eventEst == TRUE) {

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

    testg <- gof(sim = regObj$fitted.values, obs = regObj$y)

    newSummary <- c(input$yID,
                    input$xID,
                    input$lag1,
                    "",
                    "",
                    "event",
                    "gam regression",
                    dplyr::if_else(input$adjKnots == FALSE, "Default", input$knots),
                    signif(summary(regObj)$r.sq, 3),
                    testg[6,1],
                    round((sum(10^(resid(regObj))))/nobs(regObj),3),
                    paste0(input$regDateSt, " to ", input$regDateEn),
                    paste0(input$estDateSt, " to ", input$estDateEn),
                    dplyr::if_else(input$smooth == FALSE, "No", "Yes"),
                    dplyr::if_else(input$smooth == FALSE, "", paste0(input$regDateSt, " to ", input$regDateEn)),
                    dplyr::if_else(input$givePeakQ == FALSE, "", input$peakToUse),
                    dplyr::if_else(input$givePeakQ == FALSE, "", input$peakDate),
                    dplyr::if_else(input$giveObsQ == FALSE, "", input$ObsQ),
                    dplyr::if_else(input$giveObsQ == FALSE, "", input$ObsQDate))

  }

  cat(paste0(newNames, " = ", newSummary, "\n"))

})
