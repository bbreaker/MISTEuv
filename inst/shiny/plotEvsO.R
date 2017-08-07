output$plotEvsO <- renderPlot({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat(estDat, regDat)
  
  forPlot <- datP[!is.na(datP$Flow.y),]
  
  p <- ggplot(data = datP, aes(x = Flow.y, y = Estimated)) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0,max(forPlot$Estimated)), labels = comma) +
    scale_x_continuous(limits = c(0,max(forPlot$Estimated)), labels = comma) +
    stat_smooth(method = "lm", se = FALSE) + 
    labs(x = "Observed discharge, in cubic feet per second", 
         y = "Estimated discharge, in cubic feet per second") +
    theme_bw() + theme(legend.title = element_blank())
  
  p
  
})