
#' Convert image files into matrix from folder. Each row corresponds to one image
#'
#' @param inputFolder Folder that content supported image bmp, jpeg, bnp. 
#' @param w The resized width of image (pixels).
#' @param h The resized length of image (pixels)
#' @param class class label as integer (1, 2, 3,...).
#'
#' @return a data frame. The first column is the label of each image. the other column are the value of the pixels.
#' @export
#'
#' @examples
#' \dontrun{
#' images2matrix("folder/path/", w= 28, h = 28, class= 1)
#' }
#' 
#' @import EBImage
images2matrix <- function(inputFolder, w= 28, h = 28, class){
  
  names_files <- list.files(inputFolder)
  nfiles <- length(list.files(inputFolder))
  
  df <- data.frame(matrix(ncol = (w*h)+1, nrow = 0))
  # Set names. The first column is the labels, the other columns are the pixels.
  colnames(df) <-  c("Labels", paste("pixel", c(1:(w*h) )))
  
  
  for(k in 1:nfiles){
    ## get image name
    nameFile <- names_files[k]
    # load image 
    tmp_img <- EBImage::readImage(paste0(inputFolder,nameFile, sep=""))
    
    ## convet to grayscale
    tmp_img <- EBImage::channel(tmp_img, mode = 'gray')
    
    # Resize image to 28x28 pixels
    ims <- EBImage::resize(tmp_img, w = w, h = h)
    ## reduce the size of image 1/2
    #ims <- EBImage::resize(tmp_img, dim(tmp_img)[1] * scale)
    
    # Get image matrix (there should be another function to do this faster and more neatly!)
    img_matrix <- ims@.Data
    #print(dim(img_matrix))
    
    # Coerce to a vector
    img_vector <- as.vector(img_matrix)
    
    
    # Add label
    label <- class
    vec <- c(label, img_vector)
    # Stack in rs_df using rbind
    ## AVoid rbind, it ignore empty dataframe and colnames
    #df <- rbind(df_bkp, vec)
    
    df[nrow(df)+1,] <- vec
    
    # Print status
    print(paste(k,names_files[k],sep = " "))
    
  }
  
  rownames(df) <- NULL
  return(df)

}



## 
#' Visualize image from vector (a row from dataframe)
#' @usage vec2img(df, nrow, w, h)
#' @param df A dataframe WITHOUT rownames. 
#' Each row contents the pixels values of one image (as.vector). 
#' the length of the vector must to be the same for all images.
#' @param nrow numeric, the number of row want to be plot.
#' 
#' @param w wide of the image in pixels
#' @param h height of the image in pixels
#'
#' @return plot image
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(2500, mean=65, sd=4.58), nrow= 25, ncol = 100)
#' vec2img(mat, 3, 10,10)
#' }
#' 
vec2img <- function(df, nrow, w, h){
  
  #i <- imager::as.cimg(as.numeric(df[nrow,]), w, h)
  #i <- imager::rotate_xy(i, 20, 50,40)
  
  i <- EBImage::Image(as.numeric(df[nrow,]))
  dim(i) <- c(w,h, 1)
  i <- EBImage::resize(i, w= w, h= h)
 
 
  #i <- EBImage::flip(i)
  #i <- EBImage::flop(i)
  #i <- EBImage::rotate(i, 90)
  
  plot(i) ## ,axes=FALSE,layout="row" ::: if we use imager::as.cimg(df[1,])
}




## 

#' Function to split image into x/y portion and arrange them into dataframe. 
#' @usage im_split_df(path, X, Y)
#' @param path path to image file (jpeg, png, bmp)
#' @param X integer, split X times image by x axis
#' @param Y integer, split Y times image by y axis
#'
#' @return a list of a dataframe each row corresponds to one image. 
#' w and h , the wide and the height of originale image
#' @export
#'
#' @examples
#' \dontrun{
#' im_split_df("path/image.jpg", 4, 3)
#' }
im_split_df <- function(path,X,Y){
  
  im <- imager::load.image(path)
  ## convert to gray scale
  im <- imager::grayscale(im, method = "Luma", drop = TRUE)
  
  #Split pixset along x
  limx <- imager::imsplit(im,"x", X)  # %>% plot(layout="row")
  # split le list of  pixsets (x) in y along y 
  
  limxy <- lapply( limx, function(x) imager::imsplit(x,"y",Y)) 
  
  ## loop to convert a list of cimg to matrix
  
  ## get wide of the first image
  w <- dim(limxy[[1]][[1]][,,,1])[1]
  ## get height of the first image
  h <- dim(limxy[[1]][[1]][,,,1])[2]
  ## init empty df
  df <- data.frame(matrix(ncol = (w*h)+1, nrow = 0))
  
  # Set names. The first column is the labels, the other columns are the pixels.
  colnames(df) <-  c("Labels", paste0("pixel", c(1:(w*h))))
 
   for(k in 1:length(limxy)){
    
    for(i in 1:length(limxy[[k]])){
      

      
      ## This step is necessary because imsplit output  doesn't give the same size for all image. 
      im_resize <- imager::resize(limxy[[k]][[i]], w, h)
      
      # Coerce to a vector
      img_vec <- as.vector(im_resize)
      #img_vec <- as.vector(limg[[k]][[i]][,,,1])
      
      vec <- c(paste0(k,i), img_vec)
      
      df[nrow(df)+1, ] <- vec
    }
  }
  return(list(df = df, wide = w, height = h))
}
