
## visualize image from vector
# vec2img <- function(df, nrow){
# 
#   ##  plot(EBImage::Image(x_test[1,], dim = c(28,28) ))
# 
#   i <- EBImage::Image(as.numeric(df[nrow,]))
#   sqr <- sqrt(length(df[nrow,]))
#   dim(i) <- c(sqr,sqr, 1)
#   i <- EBImage::resize(i, w= 140, h= 140)
# 
#   plot(i)
#   return(i)
# }
#vec2img(class1_mat[,-1], 65)



## render image  for tested classes

lapply(1:15, function(i){
  output[[paste0('display_tested_image',i)]] <- renderPlot({
   
    ## generate index for each class/folder
    key0 <- strsplit(r_data$listFolders, ",")
    df.index <- data.frame(index = rep(seq_along(key0), sapply(key0, length)), ID = unlist(key0))
    idx <- df.index[which(df.index[,2] == input$tested_classID),1]
    
    
    ## we would like to select classes 3 
    selected_vec <- names(r_data$test_y[r_data$test_y == idx])
    
   # selected_vec_bkp <<- selected_vec
    #Test_bkp <<- r_data$Test
    #test_y_bkp <<- r_data$test_y
    
      vec2img(r_data$Test[selected_vec,][,-1],i,w= 28, h= 28)
      
  

  })
})



## render images for predicted classes

lapply(1:15, function(i){
  output[[paste0('display_predicted_image',i)]] <- renderPlot({
    
    ## generate index for each class/folder
    key0 <- strsplit(r_data$listFolders, ",")
    df.index <- data.frame(index = rep(seq_along(key0), sapply(key0, length)),ID = unlist(key0))
    idx <- df.index[which(df.index[,2]== input$predicted_classID),1]
    
    selected_vec <- which(r_data$predicted_labels == idx)
    
    vec2img(r_data$mat_for_prediction[selected_vec,][,-1],i,w= 28, h= 28)
    
    
    ## we would like to select classes 3 
    #selected_vec <- r_data$pred_y[r_data$pred_y == idx]
    
    #selected_vec_pred_bkp <<- selected_vec
    #pred_x_bkp <<- r_data$pred_x
    #pred_y_bkp <<- r_data$pred_y
    
    #vec2img(r_data$pred_x[selected_vec,][,-1],i)
    
    
    
  })
})