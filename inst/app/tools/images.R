
## visualize image from vector
vec2img <- function(df, nrow){
  
  ##  plot(EBImage::Image(x_test[1,], dim = c(28,28) ))
  
  i <- EBImage::Image(as.numeric(df[nrow,]))
  sqr <- sqrt(length(df[nrow,])) 
  dim(i) <- c(sqr,sqr, 1)
  i <- EBImage::resize(i, w= 156, h= 156)
  
  plot(i)
}
#vec2img(class1_mat[,-1], 65)



## render multiple Image using lapply

lapply(1:15, function(i){
  output[[paste0('display_image',i)]] <- renderPlot({
   
    ## genrate index for each class/folder
    key0 <- strsplit(r_data$listFolders, ",")
    df.index <- data.frame(index = rep(seq_along(key0), sapply(key0, length)),ID = unlist(key0))
    idx <- df.index[which(df.index[,2]== input$classID),1]
    
    
    ## we would like to select classes 3 
    selected_vec <- names(r_data$test_y[r_data$test_y == idx])
    
    selected_vec_bkp <<- selected_vec
    Test_bkp <<- r_data$Test
    test_y_bkp <<- r_data$test_y
    
    #class(test[selected_vec,][,-1][1:5, 1:6])
    #for(i in seq_along(selected_vec)){
      vec2img(r_data$Test[selected_vec,][,-1],i)
      
    #}

    # ##  with selecting report file input$reports_Med from sidebar menu
    # image_file <- paste(paste0(readDirectoryInput(session, 'directory'), input$periodeId_Med, sep=""),
    #                     "/", input$reports_Med[i] ,sep="")
    # 
    # 
    # return(list(
    #   src = image_file,
    #   filetype = "image/jpg",
    #   height = "1000%",
    #   width = "100%"
    # ))
    
 # }, deleteFile = FALSE)
  })
})