
#' Launch papSmear in the default browser
#'
#' @usage papSmear()
#' 
#' 
#' @return  web page of papSmear web browser
#' @examples
#' \dontrun{
#' library(papSmear)
#' papSmear()
#'}
#' @details See \url{https://papSmear.github.io/docs} for documentation and tutorials
#'
#'@import shiny
#'@import shinydashboard
#'@import magrittr
#'
#' @export
papSmear <- function(){
  if (!"package:papSmear" %in% search())
    if (!require(papSmear)) stop("Calling papSmear start function but papSmear is not installed.")
    runApp(system.file("app", package = "papSmear"), launch.browser = TRUE)
}

#' CNN
#'
#' @name CNN
#' @description Trained Convolution Neuronal Network on images of unique cervix cells
#'              The model is build with mxnet package using 2 convolution  and 2 fully connected layers
#'
#' @details 90 rounds, 20 images per round, 7 folders that countain 7 cells classes
#' @docType data
#' @author Karim Mezhoud \email{kmezhoud@gmail.com}
#' @usage data(CNN)
#' @keywords data
NULL
