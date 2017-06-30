output$plotEvsO <- renderPlot({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(estDat, regDat)
  
  p <- ggplot(data = datP, aes(x = Flow.y, y = Estimated)) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0,max(datP$Estimated))) +
    scale_x_continuous(limits = c(0,max(datP$Estimated))) +
    stat_smooth(method = "lm") + 
    labs(x = "Observed discharge, in cubic feet per second", 
         y = "Estimated discharge, in cubic feet per second") +
    theme_bw() + theme(legend.title = element_blank())
  
  p
  
})