
shinyServer(function(input, output, session) {
  
  ## stop shiny app when browser is closed
  session$onSessionEnded(function() {
    stopApp()
    print("Finished.")
  })
  source('global.R')
  
  
  ## 'sourcing' radiant's package functions in the server.R environment
  
  if (!"package:papSmear" %in% search() &&
      getOption("papSmear.path") == "..") {
    ## for shiny-server and development
    for (file in list.files("../../R", pattern="\\.(r|R)$", full.names = TRUE))
      source(file, encoding = getOption("papSmear.encoding"), local = TRUE)
  }else {
    ## for use with launcher
   # radiant.data::copy_all(radiant.dose)
  }
  
  ## source tools from papSmear app
  for (file in list.files(c("tools"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("papSmear.encoding"), local = TRUE)
  
})