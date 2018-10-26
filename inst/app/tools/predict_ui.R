## list folders of training images
output$predictDirectorylist = renderText({
  readDirectoryInput(session, 'predictDirectory')
})

# ## display folders of training images (size and number)
# output$predictFolders <- DT::renderDataTable({
#   ## compute the number of image in each folder
#   #jpeg.png <- matrix(sapply(list.files( readDirectoryInput(session, 'predictDirectory'), full.names = TRUE),
#   #                         function(x) length(list.files(x, pattern = "*.jpeg|*.png"))))
#   jpeg.png <- matrix(length(list.files(readDirectoryInput(session, 'predictDirectory'), pattern = "*.jpeg|*.png")))
#   
#   files = list.files(readDirectoryInput(session, 'predictDirectory'), full.names = TRUE)
#   data.frame(name = basename(files) , file.info(files)[1], jpeg.png)
# }, caption= paste('Path of folder: ', shiny::textOutput('predictDirectorylist'))
# )

output$notif_predict_nfiles <- renderText({
  n.img <- length(list.files(readDirectoryInput(session, 'predictDirectory'), pattern = "*.jpeg|*.png"))
  if(n.img == 0){
    paste0("jpeg/png files: ",   "<font color=red><b>", n.img, "</b></font>")
  } else{
    paste0("jpeg/png files: ", "<font color=blue><b>", n.img, "</b></font>")
  }
})


output$ui_img2matrixSB_pred <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.predict_SBID ==false",
                           tags$b('Predict',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.predict_SBID == true",
                           tags$b('Predict',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "predict_SBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})

output$predict_result_mxnet.CNN <- renderTable({
  if(!is.null(r_data$predictResult_mxnet.CNN)){
    ## convert table to dataframe 
    df <- as.data.frame.matrix(r_data$predictResult_mxnet.CNN)
    colnames(df) <- paste0("pred_c", colnames(df))
    
    row.names(df) <- paste0("c", rownames(df))
    
    df %>% tibble::rownames_to_column("predicted")
  }
})

output$ui_viewPredImgSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.view_predImgSBID ==false",
                           tags$b('View Predicted Class',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.view_predImgSBID == true",
                           tags$b('View Predicted Class',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "view_predImgSBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})

output$ui_predict <- renderUI({
  fluidPage(
    fluidRow(
      
      column( width = 4,
              box( class= "sand", width = 12, title = "Select images folder", collapsible = TRUE,
                   collapsed = FALSE,   solidHeader = TRUE, #background = "blue",
                   tabItem(tabName = "inputPredictImagesFolder",
                           wellPanel(
                             directoryInput('predictDirectory', label = 'Select Images Folder',
                                            value = getwd()), 
                             htmlOutput("notif_predict_nfiles"),
                             uiOutput("ui_img2matrixSB_pred"),
                             conditionalPanel("input.predict_SBID == true",
                                              selectizeInput("predicted_classID", label = "View predicted class:",
                                                             choices = r_data$listFolders,
                                                             selected = "", multiple = FALSE),
                                              
                                              uiOutput("ui_viewPredImgSB")
                                              ),
                             # selectizeInput("tested_classID", label = "View tested class:", choices = r_data$listFolders,
                             #                selected = "", multiple = FALSE),
                             # 
                             # uiOutput("ui_viewTestedImgSB"),
                             
                             div(class="row",
                                 div(class="col-xs-6"
                                     
                                     
                                 ),
                                 div(class= "col-xs-6"
                                     
                                 )
                                 
                             )
                           )
                   )
              )),
      column(width = 8,
             box(class= "sand", title = "Training directory information", width = 14 , collapsible = TRUE,
                 collapsed = FALSE, solidHeader = TRUE,
                 
                 #shiny::textOutput('trainDirectorylist'),
                 #div(DT::dataTableOutput('predictFolders'), style = "font-size: 75%; width: 80%; overflow-y: scroll")
                 conditionalPanel("input.predict_SBID == true",
                 div(tableOutput("predict_result_mxnet.CNN"),
                     style = "font-size: 75%; width: 80%; overflow-y: scroll; align:center")
                 )
             )
             
      )#,
      # column(width = 4,
      #        
      #        box(class = "sand", title = "Datasets informations", width = 14 , collapsible = TRUE,
      #            collapsed = FALSE, solidHeader = TRUE
      #            
      #            
      #        )
      #        
      # )
    ),
    fluidRow(
      
      tagList(
        conditionalPanel("input.view_predImgSBID == true",
                         
                         # splitLayout(
                         lapply(1:15, function(i){
                           
                           div(style="display:inline-block",
                               tags$a(plotOutput(paste0("display_predicted_image",i),
                                                 width= 110, height = 110), # input$zoom_MedicalReport
                                      href="https://www.google.com" )
                               #helpText( a("Click Here for the Source Code on Github!",
                               #           href="https://github.com/Bohdan-Khomtchouk/Microscope",target="_blank"))
                           )
                         })
                         #)
        )
      )
    ),
    fluidRow(
      
      
      tagList(
        # conditionalPanel("input.view_testImgSBID == true",
        #                  
        #                  # splitLayout(
        #                  lapply(1:15, function(i){
        #                    
        #                    div(style="display:inline-block",
        #                        tags$a(plotOutput(paste0("display_tested_image",i),
        #                                          width= 110, height = 110), # input$zoom_MedicalReport
        #                               href="https://www.google.com" )
        #                        #helpText( a("Click Here for the Source Code on Github!",
        #                        #           href="https://github.com/Bohdan-Khomtchouk/Microscope",target="_blank"))
        #                    )
        #                  })
        #                  #)
        # )
      )
    )
  )
})