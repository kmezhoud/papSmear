
## listen setting input folders that content images to convert
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$convertInputDirectory
  },
  handlerExpr = {
    if (input$convertInputDirectory > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'convertInputDirectory'))
      updateDirectoryInput(session, 'convertInputDirectory', value = path)
    }
  }
)

## listen setting ouput folders to save converted images
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$convertOutputDirectory
  },
  handlerExpr = {
    if (input$convertOutputDirectory > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'convertOutputDirectory'))
      updateDirectoryInput(session, 'convertOutputDirectory', value = path)
    }
  }
)

#   conditionalPanel("input.runConvertImageSBId == true", 
# convert_image(inputFolder = input$convertInputDirectory,
#               outputFolder = input$convertOutputDirectory)
# )
output$convertImage <- renderPrint({
  
  withProgress(message = 'Converting Images... ', value = 0.1, {
    Sys.sleep(0.25)
    #print(getwd())
    #readDirectoryInput(session, 'convertInputDirectory')
    #print(input$convertInputDirectory)
    robjname <- try(
    convert_image(inputFolder =  readDirectoryInput(session, 'convertInputDirectory'),
                  outputFolder = readDirectoryInput(session, 'convertOutputDirectory'))
  , silent = TRUE)
    if (is(robjname, "try-error")) {
      showNotification(ui = "Select a valid folder that content jpeg or png or bmp images.", id = "convertErrorID",
                       duration = NULL, type = "warning")
      input$runConvertImageSBId == 0
      
    }else{
      showNotification(ui = "The image convertion was done.",  type = "message")
    }
    
  })
})