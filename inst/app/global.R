
options(papSmear.encoding = "UTF-8")

## define global
r_data <- reactiveValues()

## set path for package
ifelse(grepl("papSmear", getwd()) && file.exists("inst") , getwd(), 
       
       ## set home as work directory
       '~')  %>%
       ## set workspace in package folder
       #system.file(package = "papSmear")) %>%    # for server
  options(papSmear.path = .)

## loading urls and ui
#source("init.R", getOption(papSmear.encoding), local = TRUE)



## check if a button was NOT pressed
not_pressed <- function(x) ifelse(is.null(x) || x == 0, TRUE, FALSE)