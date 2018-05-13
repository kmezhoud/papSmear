
## listen setting predict image directory
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$predictDirectory
  },
  handlerExpr = {
    if (input$predictDirectory > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'predictDirectory'))
      updateDirectoryInput(session, 'predictDirectory', value = path)
      ## read folders
      r_data[["predictDirectory"]] <- list.files(readDirectoryInput(session, 'predictDirectory'),  full.names = TRUE)
      
    }
  }
)

observe({
  if (not_pressed(input$predict_SBID))  return()
  isolate({
    #if(length(r_data$classes) == 0){
      
      folderPath <- readDirectoryInput(session, 'predictDirectory')
      
      withProgress(message = 'Reading Images... ', value = 0.4, {
        Sys.sleep(0.25)
        
  
          ## try file 
          robjname <- try(
            df<- images2matrix(folderPath, w= 28, h= 28, class= 1)
            , silent = TRUE)
          if (is(robjname, "try-error")) {
            showNotification(ui = "Select a valid folder that content jpeg or png or bmp images.", 
                             id = "readingImgErrorID",
                             duration = NULL, type = "warning")
            
            ## reset switchButton of predict_SBID
            input$predict_SBID == 0
           
          }
          
           r_data[['mat_for_prediction']] <- df
        
          if(input$ModelId == 'mxnet_CNN'){
      
            ## Setup  predict arrays
            pred <- data.matrix(df)
            pred_x <- t(pred[, -1])
            pred_y <- pred[, 1]
            pred_array <- pred_x
            dim(pred_array) <- c(28, 28, 1, ncol(pred_x))
            
            r_data[["pred_array"]] <- pred_array
            r_data[["pred_y"]] <- pred_y
            
          }else if(input$ModelId == 'keras_MLP'){
            
            
            pred <- data.matrix(df)
            pred_x <- data.matrix(pred[,-1])
            r_data[["pred_x"]] <- pred_x
            pred_y <- as.integer(pred[,1])
            ## Prepare this data for training we [one-hot](https://www.quora.com/What-is-one-hot-encoding-and-when-is-it-used-in-data-science) 
            ## encode the vectors into binary class matrices
            pred_y <- keras::to_categorical(pred_y, (1+1))[,-1]  ## 8-1 length of classes 
            
            r_data[["pred_y"]] <- pred_y
          }
      })
      withProgress(message = 'Predicing Images... ', value = 0.4, {
        Sys.sleep(0.25)
      
      if(!is.null(r_data$trained_model_mxnetCNN)){  #  && input$trained_model != 0
        
        # predict.MXFeedForwardModel function is not exported by mxnet
        predicted <- mxnet:::predict.MXFeedForwardModel(model = r_data$trained_model_mxnetCNN, 
                                                        X = r_data$pred_array)
        
        #predicted <- predict(model = r_data$trained_model_mxnetCNN, 
         #                                               X = r_data$pred_array)
        # Assign labels
        predicted_labels <- max.col(t(predicted)) - 1
        
        r_data[["predicted_labels"]] <- predicted_labels
        
        predicted_labels_bkp <<- predicted_labels
        # Get accuracy
        r_data[["predictResult_mxnet.CNN"]] <- table(r_data$pred_y, predicted_labels)
        
        
      }else if(!is.null(r_data$trained_model_kerasMLP)){
        
        ## eval model
        eval <- r_data$trained_model_kerasMLP %>% keras::evaluate(r_data$pred_x, r_data$pred_y)
        #eval <- data.frame(eval)
        # list
        # 15/15 [==============================] - 0s 1ms/step
        # $loss
        # [1] 1.706367
        # 
        # $acc
        # [1] 0.7333333
        
        ## prediction
        result <- r_data$trained_model_kerasMLP %>% keras::predict_classes(r_data$pred_x)
        #result <- as.vector(result)
        #array
        #[1] 0 0 0 0 0 1 1 1 1 1 1 1 2 1 1
        r_data[["result_keras.MLP"]] <- list(evaluation = unlist(eval), prediction= result)
      }
          
      })
  })
})