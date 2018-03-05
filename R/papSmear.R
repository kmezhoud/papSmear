
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