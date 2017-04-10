MISTEuvgui <- function() {
  
  appDir <- system.file("shiny", package = "MISTEuv")
  
  if (appDir == "") {
    
    stop("Could not find GUI directory. Try re-installing `MISTEuv`.", call. = FALSE)
    
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
  
}
