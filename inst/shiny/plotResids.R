output$plotResids <- renderPlot({

  estDat <- getEstDat()

  regDat <- getRegDat()

  datP <- allMISTEdat(regDat, estDat)

  if (input$eventEst == FALSE) {

    if (input$use2 == FALSE) {

      if (input$Method == 1) {

        regObj <- lm(log10(Flow.y) ~ log10(Flow.x), data = regDat)

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

      }

      else if (input$Method == 2) {

        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts"), data = regDat, select = TRUE)

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

      }

    }

    else if (input$use2 == TRUE) {

      if (input$Method == 1) {

        regObj <- lm(log10(Flow.y) ~ log10(Flow.x) + log10(Flow.x2), data = regDat)

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

      }

      else if (input$Method == 2) {

        regObj <- gam(log10(Flow.y) ~ s(log10(Flow.x), bs = "ts") +
                        s(log10(Flow.x2), bs = "ts"), data = regDat, select = TRUE)

        regDF <- augment(regObj)

        p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
          stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
          scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
          labs(x = "Fitted values", y = "Residuals") +
          theme_bw()

        p

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
    
    regDF <- augment(regObj)

    p <- ggplot(regDF, aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
      stat_smooth(method = "loess", span = 0.9, color = "red", linetype = "dashed", se = FALSE) +
      scale_y_continuous(limits = c(-1*max(abs(regDF$.resid)), 1*max(abs(regDF$.resid)))) +
      labs(x = "Fitted values", y = "Residuals") +
      facet_wrap(~comment) +
      theme_bw()

    p

  }

})
