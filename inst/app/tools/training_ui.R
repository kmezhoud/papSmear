

## list folders of training images
output$trainDirectorylist = renderText({
  readDirectoryInput(session, 'trainDirectory')
})

## display folders of training images (size and number)
output$trainFolders <- renderTable({
  ## compute the number of image in each folder
  jpeg.png <- matrix(sapply(list.files( readDirectoryInput(session, 'trainDirectory'), full.names = TRUE),
                            function(x) length(list.files(x, pattern = "*.jpeg|*.png"))))
  
  files = list.files(readDirectoryInput(session, 'trainDirectory'), full.names = TRUE)
  data.frame(name = basename(files) , file.info(files)[1], jpeg.png)
}, caption= paste('Path of folder: ', shiny::textOutput('trainDirectorylist'))
)

output$ui_trainModelSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.trainModelSBID ==false",
                           tags$b('Train the model',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.trainModelSBID == true",
                           tags$b('Train the model',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "trainModelSBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})


output$ui_img2matrixSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.img2matrixSBID ==false",
                           tags$b('Reshape Datasets',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.img2matrixSBID == true",
                           tags$b('Reshape Datasets',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "img2matrixSBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})

output$img2matrix_notification <- renderTable({
  
  #paste("N° classes:", length(r_data$classes))
  #paste( "Dimension of selected data:" ),
  if(length(r_data$classes) > 0){
    df <- data.frame(sapply(r_data$classes, dim))
    df <- cbind(" "= c("samples", "pixels"), df)
    df
  }else{
    df <- data.frame(NULL)
  }
}, caption= 'Datasets informations ',
caption.placement = getOption("xtable.caption.placement", "top")
)

##################################################
###########   Compile Models   ###################
##################################################


output$ui_compileModelSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.compileModelSBID ==false",
                           tags$b('Compile the model',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.compileModelSBID == true",
                           tags$b('Compile the model',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "compileModelSBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})


output$compileModel_mxnet.CNN_notification <- renderText({
  if(!is.null(r_data$mxnetCNN_model)){
    summary(r_data$mxnetCNN_model)
  }
})

output$compileModel_keras.MLP_notification <- renderTable({
  if(!is.null(r_data$kerasMLP_model)){
    
    a <- gsub({"_*||=*"},"",strsplit(as.character(r_data$kerasMLP_model), "\n")[[1]])
    b <- strsplit(a[grep(" ",a)],split="   *")
    data.frame(t(matrix(unlist(b), nrow = 3)))
    
  }
})


output$trainedModel_notification <- renderTable({
  if(!is.null(r_data$trained_model_mxnetCNN)){
    
    obj <- summary(r_data[["trained_model_mxnetCNN"]]$arg.params)
    data.frame(do.call(rbind, list(obj))) %>% 
      tibble::rownames_to_column("Layers")
    
  } else if(!is.null(r_data$trained_model_kerasMLP)){
    
    data.frame(acc= max(r_data[["trained_model_kerasMLP_metrics"]]$metrics$acc),
               val_acc= sd(r_data[["trained_model_kerasMLP_metrics"]]$metrics$val_acc),
               loss = min(r_data[["trained_model_kerasMLP_metrics"]]$metrics$loss),
               val_loss = sd(r_data[["trained_model_kerasMLP_metrics"]]$metrics$val_loss))
  }
  
})

##################################################
###########   Test Models   ###################
##################################################

output$ui_testSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.testSBID ==false",
                           tags$b('Test the model',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.testSBID == true",
                           tags$b('Test the model',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "testSBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})

output$test_result_mxnet.CNN <- renderTable({
  if(!is.null(r_data$result_mxnet.CNN)){
    ## convert table to dataframe 
    df <- as.data.frame.matrix(r_data$result_mxnet.CNN)
    colnames(df) <- paste0("pred_c", colnames(df))
    
    row.names(df) <- paste0("c", rownames(df))
    
    df %>% tibble::rownames_to_column("tested")
  }
})


output$test_result_keras.MLP <- renderPrint({
  if(!is.null(r_data$result_keras.MLP)){
    
    print(r_data[["result_keras.MLP"]])
    
  }
  
})



output$ui_viewTestedImgSB <- renderUI({
  
  div(class="row",
      div(class="col-xs-8",
          conditionalPanel(condition = " input.view_testImgSBID ==false",
                           tags$b('View tested class',  style = "color:#7f7f7f")),
          conditionalPanel(condition = " input.view_testImgSBID == true",
                           tags$b('View tested class',  style = "color:#428bca"))
      ),
      div(class="col-xs-4",
          # conditionalPanel(condition = "input.StudiesIDCircos != null",
          switchButton(inputId = "view_testImgSBID",
                       value = FALSE, col = "GB", type = "OO")
          #)
      )
  )
})

output$ui_training <- renderUI({
  fluidPage(
  fluidRow(
    
    column( width = 4,
            box( class= "sand", width = 12, title = "Select training directory", collapsible = TRUE,
                 collapsed = FALSE,   solidHeader = TRUE, #background = "blue",
                 tabItem(tabName = "inputTrainingImagesFolder",
                         wellPanel(
                           directoryInput('trainDirectory', label = 'Select Images Folder',
                                          value = getwd()),  
                           
                           div(class="row",
                               div(class="col-xs-6",
                                   numericInput("numberTestingId", "Test image N°",
                                                min = 5, max = 20, value = 10, step = 5)
                                   
                               ),
                               div(class= "col-xs-6",
                                   numericInput("resolutionId", "Image resolution",
                                                min = 28, max = 128, value = 28, step = 28)
                               )
                               
                           )
                         ),
                         uiOutput("ui_img2matrixSB")
                         
                 )
            )),
    column(width = 4,
           box(class= "sand", title = "Training directory information", width = 14 , collapsible = TRUE,
               collapsed = FALSE, solidHeader = TRUE,
               
               #shiny::textOutput('trainDirectorylist'),
               div(tableOutput('trainFolders'), style = "font-size: 75%; width: 80%; overflow-y: scroll")
               
           )
           
           ),
    column(width = 4,
           
           box(class = "sand", title = "Datasets informations", width = 14 , collapsible = TRUE,
               collapsed = FALSE, solidHeader = TRUE,
               # wellPanel(
               conditionalPanel("input.img2matrixSBID == true", 
                                div(tableOutput('img2matrix_notification'),
                                    style = "font-size: 75%; width: 100%; overflow-y: scroll")
                                #  )
               )
               
           )
           
           )
    ),
  fluidRow(
    column(  width = 4,
            box(class= "sky", width = 12, title = "Set the Model", collapsible = TRUE,
                collapsed = FALSE,   solidHeader = TRUE, #background = "blue",
                tabItem(tabName = "inputSetModel",
                        
                        wellPanel(
                          selectInput(inputId = 'ModelId',label = 'Model Type',
                                      choices = c('mxnet_CNN', 'keras_MLP'), selected = ""),
                          
                          selectInput(inputId = "activationID", label = "Activation Type",
                                      choices = c('tanh', 'relu'), selected = "tanh")
                          
                        ),
                        uiOutput("ui_compileModelSB")
                        
                )
            )
            ),
    column(width = 4,
           box(class= "sky",title = "Model features", width = 14 , collapsible = TRUE,
               collapsed = FALSE, solidHeader = TRUE,
               
               wellPanel(
                 conditionalPanel("input.compileModelSBID == true && input.ModelId == 'mxnet_CNN'",
                                  textOutput("compileModel_mxnet.CNN_notification")
                 ),
                 conditionalPanel("input.compileModelSBID == true && input.ModelId == 'keras_MLP'", 
                                  div(tableOutput("compileModel_keras.MLP_notification"),
                                      style = "font-size: 75%; width: 100%; overflow-y: scroll")
                 )
               )
               
           )
           )
    ),
  fluidRow(
    column( width = 4,
    
            box(class= "lavende",  width = 12, title = "Train the Model", collapsible = TRUE,
                collapsed = FALSE,   solidHeader = TRUE, #background = "blue",
                tabItem(tabName = "inputTrainModel",
                        
                        
                        wellPanel(
                          div(class="row",
                              div(class="col-xs-6",
                                  numericInput("numb.roundID", label = "N° Rounds",
                                               min = 10, max = 300, value = 100, step = 10)
                                  
                              ),
                              div(class= "col-xs-6",
                                  numericInput("array.batch.sizeID", label = "Images/round",
                                               min = 10, max = 256, value = 20, step = 5)
                              )
                          ),
                          
                          selectInput(inputId = "optimizerID", label = "Optimizer Mode",
                                      choices = c('sgd', 'adam', 'RMSprop'), selected = "sgd")
                          
                        ),
                        uiOutput("ui_trainModelSB"),
                        uiOutput("ui_testSB")
                        
                )
            )
            
    ),
    
    column( width = 4,
            
            box(class= "lavende",  title = "Trained Model features", width = 14 , collapsible = TRUE,
                collapsed = FALSE, solidHeader = TRUE,
                
                wellPanel(
                  div(tableOutput("trainedModel_notification"),
                      style = "font-size: 75%; width: 100%; overflow-y: scroll")#,
                  #div(
                  #DT::dataTableOutput("predict_result")#,
                  #   style = "font-size: 75%; width: 100%; overflow-y: scroll")
                )
                
                
            )
            
    ),
    column(
      width = 4,
      
      box(class= "lavende", title = "Result of testing", width = 14 , collapsible = TRUE,
          collapsed = FALSE, solidHeader = TRUE,
          
          ## get means of identical labels, located in the diagonal
          
          #if(!is.null(r_data$tested_labels)){  # r_data$result_mxnet.CNN
          conditionalPanel("input.ModelId == 'mxnet_CNN'",
                            conditionalPanel("input.testSBID == true",
                           #h4("score =  ", sum(diag(table(r_data$test_y,r_data$tested_labels)))/length(r_data$test_y),
                          #   style="color:red"), # , align="center"
                           
                           div(tableOutput('test_result_mxnet.CNN'),
                               style = "font-size: 75%; width: 80%; overflow-y: scroll; align:center")
                           
          )),
          #  }else if(!is.null(r_data$result_keras.MLP)){
          conditionalPanel("input.ModelId == 'keras_MLP'",
                           
                           textOutput("test_result_keras.MLP")
                           
          )
          # }
          
      )
    )
    ),
    fluidRow(
    column( width = 4,
            box(class= "lavende", title = "View tested classes", width = 14 , collapsible = TRUE,
                collapsed = FALSE, solidHeader = TRUE
                
                ## update list of classes/folders
                #updateSelectInput(session, "tested_classID",choices = r_data$listFolders,selected = "")
                
                #  selectizeInput("tested_classID", label = "View tested class:", choices = r_data$listFolders,
                #                 selected = "", multiple = FALSE),
                # 
                # uiOutput("ui_viewTestedImgSB")
          
                  )
      
    ),
    column(width = 8
           # tagList(
           # conditionalPanel("input.view_testImgSBID == true",
           #                  
           #                 # splitLayout(
           #                   lapply(1:10, function(i){
           # 
           #                    div(style="display:inline-block",
           #                        tags$a(plotOutput(paste0("display_tested_image",i),
           #                                           width= 110, height = 110), # input$zoom_MedicalReport
           #                               href="https://www.google.com" )
           #                        #helpText( a("Click Here for the Source Code on Github!",
           #                        #           href="https://github.com/Bohdan-Khomtchouk/Microscope",target="_blank"))
           #                    )
           #                  })
           #                  #)
           # )
           # )
           
           
           )
    )
    )
    
    
# 
#     column(
#       width = 4
#       
#       
#       # output$mymarkdown <- renderUI({  
#       #   rmarkdown::render(input = "../extdata/Dumas.Rmd",
#       #                     output_format = html_document(self_contained = TRUE),
#       #                     output_file = 'mymarkdown.html')  
#       #   shiny::includeHTML('../extdata/mymarkdown.html') 
#       # }) 
#       # box(title = "rendering rmarkdown", width = 8 , collapsible = TRUE,
#       #     collapsed = TRUE, solidHeader = TRUE,
#       #     div(style="overflow-y: scroll",
#       #         uiOutput('mymarkdown')
#       #     )
#       #     )
#     )
  
})