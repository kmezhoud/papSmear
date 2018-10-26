#' Convert image to jpeg or png
#'
#' @param inputFolder Folder that content the subfolders of images with other extension that jpeg and png. 
#'                   Each subfolder corresponds to one class.
#' @param outputFolder The folder where will save cponverted images.
#' @param type  the type of extension jpeg, png all supported extesion by imager package
#'
#' @return  save images as jpeg (default) or png. Each folder corresponds to a classe.
#' @export
#'
#' @examples
#' \dontrun{
#' convert_image("/extdata/img_data/")
#' }
#' 
#' @importFrom imager load.image save.image
#' @importFrom tools file_path_sans_ext
#' 
#' @export
convert_image <- function(inputFolder, outputFolder, type = '.jpeg' ){

  names_files <- list.files(inputFolder)
  nfiles <- length(list.files(inputFolder))
  
  for(k in seq_len(nfiles)){
    ## get image name
    nameFile <- names_files[k]
    #print(nameFile)
    #print(paste(inputFolder,nameFile, sep="/"))
   # print( paste0('done: ', nameFile, '  ->  ' ,type ))
    # load image 
    tmp_img <- imager::load.image(paste(inputFolder,nameFile, sep="/"))
    # save image
    imager::save.image(im = tmp_img, file = paste0(outputFolder,'/',
                                                   tools::file_path_sans_ext(nameFile), type ),quality = 1)
  }
}