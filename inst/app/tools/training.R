

## listen setting training image directory
observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$trainDirectory
  },
  handlerExpr = {
    if (input$trainDirectory > 0) {
      # condition prevents handler execution on initial app launch
      
      path = choose.dir(default = readDirectoryInput(session, 'trainDirectory'))
      updateDirectoryInput(session, 'trainDirectory', value = path)
      ## read folders
      r_data[["listFolders"]] <- list.files(readDirectoryInput(session, 'trainDirectory'),  full.names = FALSE)
      
    }
  }
)



## Convert images to matrix
observe({
  if (not_pressed(input$img2matrixSBID))  return()
  isolate({
    if(length(r_data$classes) == 0){
      
      listFiles <- list.files(readDirectoryInput(session, 'trainDirectory'),  full.names = TRUE)
      
      ## remove double slash. THIS MAY GET ERROR WITH WINDOWS  ################
      listFiles <- gsub("//", "/", listFiles)
      
      r_data[["listFiles"]] <- listFiles
      
      withProgress(message = 'Converting Images... ', value = 0.4, {
        Sys.sleep(0.25)
        
        classes <- list()
        for(i in seq_len(length(listFiles))){
          ## try file 
          robjname <- try(
            classes[[paste0("c",i)]] <- images2matrix(paste0(listFiles[i], "/"), w= input$resolutionId, h= input$resolutionId, class= i)
            , silent = TRUE)
          if (is(robjname, "try-error")) {
            showNotification(ui = "Select a valid folder that content jpeg or png or bmp images.", id = "img2matrixErrorID",
                             duration = NULL, type = "warning")
            
            ## reset switchButton of image2matrix
            input$img2matrixSBID == 0
            break
          }
        }
      })
      r_data[["classes"]] <- classes
    }
    
  })
})


## Define Model
def_Model <- function(label){
  if(label == 'mxnet_CNN'){
    data <- mxnet::mx.symbol.Variable('data')
    # 1st convolutional layer
    conv_1 <- mxnet::mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 20)
    tanh_1 <- mxnet::mx.symbol.Activation(data = conv_1, act_type = input$activationID)
    pool_1 <- mxnet::mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
    # 2nd convolutional layer
    conv_2 <- mxnet::mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
    tanh_2 <- mxnet::mx.symbol.Activation(data = conv_2, act_type = input$activationID)
    pool_2 <- mxnet::mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
    # 1st fully connected layer
    flatten <- mxnet::mx.symbol.Flatten(data = pool_2)
    fc_1 <- mxnet::mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
    tanh_3 <- mxnet::mx.symbol.Activation(data = fc_1, act_type = input$activationID)
    # 2nd fully connected layer
    fc_2 <- mxnet::mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)
    # Output. Softmax output since we'd like to get some probabilities.
    NN_model <- mxnet::mx.symbol.SoftmaxOutput(data = fc_2)
    
    r_data[["mxnetCNN_model"]] <- NN_model
    
  }else if(label == 'keras_MLP'){
    MLP_model <- keras::keras_model_sequential() 
    MLP_model %>% 
      keras::layer_dense(units = 256, activation = input$activationID, input_shape = c(784)) %>% 
      keras::layer_dropout(rate = 0.4) %>% 
      keras::layer_dense(units = 128, activation = input$activationID) %>%
      keras::layer_dropout(rate = 0.3) %>%
      keras::layer_dense(units = length(r_data$listFiles), activation = 'softmax') ## listFiles == list of folders/classes
    
    ## Next, compile the model with appropriate loss function, optimizer, and metrics:
      MLP_model %>% 
      keras::compile( loss = 'categorical_crossentropy',
                      optimizer ='adam', # Stochastic Gradient Descent (SGD), ADAM and RMSprop.  optimizer_rmsprop()
                      metrics = c('accuracy'))
      
      r_data[["kerasMLP_model"]] <- MLP_model
  }
}


## Sampling Train and Test and compile model
observe({
  if (not_pressed(input$compileModelSBID))  return()
  isolate({
    if(length(r_data$classes) > 0){
      
      withProgress(message = 'Compiling the model ... ', value = 0.6, {
        Sys.sleep(0.25)
        
        list_smp <- merge_train_test(r_data$classes, n_test= input$numberTestingId)
        
        ## make Train and Test dataframes available for globalEnv
        r_data[['Train']] <- list_smp$allTrain
        r_data[['Test']] <- list_smp$allTest
        
        Train <- list_smp$allTrain
        Test <- list_smp$allTest
        
        
        if(input$ModelId == 'mxnet_CNN'){
        # Setup Train arrays
        train <- data.matrix(Train)
        train_x <- t(train[, -1])
        train_y <- train[, 1]
        train_array <- train_x
        dim(train_array) <- c(28, 28, 1, ncol(train_x))
        
        r_data[["train_array"]] <- train_array
        r_data[["train_y"]] <- train_y
        
        ## Setup  Test arrays
        test <- data.matrix(Test)
        test_x <- t(test[, -1])
        test_y <- test[, 1]
        test_array <- test_x
        dim(test_array) <- c(28, 28, 1, ncol(test_x))
        
        r_data[["test_array"]] <- test_array
        r_data[["test_y"]] <- test_y
        
        }else if(input$ModelId == 'keras_MLP'){
          train_x <- data.matrix(Train[,-1])
          r_data[["train_x"]] <- train_x
          train_y <- as.integer(Train[,1])
          test_x <- data.matrix(Test[,-1])
          r_data[["test_x"]] <- test_x
          test_y <- as.integer(Test[,1])
          ## Prepare this data for training we [one-hot](https://www.quora.com/What-is-one-hot-encoding-and-when-is-it-used-in-data-science) 
          ## encode the vectors into binary class matrices
          train_y <- keras::to_categorical(train_y, (length(r_data$listFiles)+1))[,-1]  ## 8-1 length of classes
          test_y <- keras::to_categorical(test_y, (length(r_data$listFiles)+1))[,-1]
          
          r_data[["train_y"]] <- train_y
          r_data[["test_y"]] <- test_y
        }
        
        ## def_Model returns r_data[["mxnetCNN_model"]] or r_data[["kerasMLP_model"]]
        def_Model(input$ModelId)
        
      })
    }
  })
})


## Train the model

observe({
  if (not_pressed(input$trainModelSBID))  return()
  isolate({
    
    if(input$ModelId == 'mxnet_CNN'){
      withProgress(message = 'Training model CNN ... ', value = 0.6, {
        Sys.sleep(0.25)
      # Pre-training set up
      #-------------------------------------------------------------------------------
      
      # Set seed for reproducibility
      mxnet::mx.set.seed(100)
      
      # Device used. CPU in my case.
      devices <- mxnet::mx.cpu()
      
      # Training
      #-------------------------------------------------------------------------------
      
      # Train the model
      model <- mxnet::mx.model.FeedForward.create(symbol = r_data$mxnetCNN_model,       # The network schema
                                           X = r_data$train_array,         # Training array
                                           y = r_data$train_y,             # Labels/classes of training dataset
                                           ctx = devices,
                                           num.round = input$numb.roundID,
                                           array.batch.size = input$array.batch.sizeID,  # number of array in the batch size
                                           learning.rate = 0.02,
                                           momentum = 0.9,
                                           optimizer = input$optimizerID,
                                           eval.metric = mxnet::mx.metric.accuracy,
                                           #initializer=mx.init.uniform(0.05),
                                           epoch.end.callback = mxnet::mx.callback.log.train.metric(100))
      
      
      r_data[["trained_model_mxnetCNN"]] <- model
      })
    }else if(input$ModelId == 'keras_MLP'){
      
      withProgress(message = 'Training model MLP ... ', value = 0.6, {
        Sys.sleep(0.25)
        
        r_data[["trained_model_kerasMLP_metrics"]] <- r_data$kerasMLP_model %>% keras::fit(
        r_data$train_x, r_data$train_y, 
        epochs = input$numb.roundID, batch_size =  input$array.batch.sizeID,  ## 256 pixels ou images?  
        validation_split = 0.2
      )
      
      r_data[["trained_model_kerasMLP"]] <- r_data$kerasMLP_model
      
      })
    }
    
    
  })
})


## Predict/ evaluat/ Test

observe({
  if(not_pressed(input$predictSBID))  return()
  isolate({
    
    if(!is.null(r_data$trained_model_mxnetCNN)){  #  && input$trained_model != 0
      
      # predict.MXFeedForwardModel function is not exported by mxnet
      predicted <- mxnet:::predict.MXFeedForwardModel(model = r_data$trained_model_mxnetCNN, 
                                                      X = r_data$test_array)
      # Assign labels
      predicted_labels <- max.col(t(predicted)) -1
      r_data[["predicted_labels"]] <- predicted_labels
      
      # Get accuracy
       r_data[["result_mxnet.CNN"]] <- table(r_data$test_y, predicted_labels)
      
      
      
    }else if(!is.null(r_data$trained_model_kerasMLP)){
      
      ## eval model
      eval <- r_data$trained_model_kerasMLP %>% keras::evaluate(r_data$test_x, r_data$test_y)
      #eval <- data.frame(eval)
      # list
      # 15/15 [==============================] - 0s 1ms/step
      # $loss
      # [1] 1.706367
      # 
      # $acc
      # [1] 0.7333333
      
      ## prediction
      result <- r_data$trained_model_kerasMLP %>% keras::predict_classes(r_data$test_x)
      #result <- as.vector(result)
      #array
      #[1] 0 0 0 0 0 1 1 1 1 1 1 1 2 1 1
      r_data[["result_keras.MLP"]] <- list(evaluation = unlist(eval), prediction= result)
    }
  })
    
  })