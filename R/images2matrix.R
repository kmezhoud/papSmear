
#' Convert images into matrix. Each row corresponds to one image
#'
#' @param inputFolder Folder that content supported image bmp, jpeg, bnp. 
#' @param w The resized width of image (pixels).
#' @param h The resized length of image (pixels)
#' @param class class label as integer.
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
