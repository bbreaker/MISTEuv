output$plotEvsO <- renderPlot({
  
  estDat <- getEstDat()
  
  regDat <- getRegDat()
  
  datP <- allMISTEdat()
  
  p <- ggplot(data = datP, aes(x = Flow.y, y = Estimated)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm") + 
    labs(x = "Observed discharge, in cubic feet per second", 
         y = "Estimated discharge, in cubic feet per second") +
    theme_bw() + theme(legend.title = element_blank())
  
  p
  
})