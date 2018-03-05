


output$ui_runConvertImagesSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.runConvertImageSBId ==false",
                           h5('Convert Images',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.runConvertImageSBId == true",
                           h5('Convert Images',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "runConvertImageSBId",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})




output$ui_convert <- renderUI({
  
  fluidRow(
    column(
      width = 4,
      box( class = "information", width = 12, title = "Select input/output folders", collapsible = TRUE,
           collapsed = FALSE,   solidHeader = TRUE, background = "yellow",
           tabItem(tabName = "convert_folders",
                   
                   directoryInput('convertInputDirectory', label = '',
                                  value = '~'),
                   
                   directoryInput('convertOutputDirectory', label = '',
                                  value = '~'),
                   
                   uiOutput("ui_runConvertImagesSB")
                   
           )
           
      )),
    column(
      width = 8,
      
      box(title = "Controls", width = 12, collapsible = TRUE, 
          collapsed = FALSE, solidHeader = TRUE,
          div(style = 'overflow-x: scroll',
          conditionalPanel(condition = "input.runConvertImageSBId ==true",
                               verbatimTextOutput("convertImage")
                           )
          )
          
      )
    )
  )
  
})
