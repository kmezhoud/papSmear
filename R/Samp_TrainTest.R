 
#' Sampling training and testing row from dataframe
#'
#' @param df dataframe with samples in the row and pixels in columns.
#'          The first column is the label or class
#' @param n_test number of test samples for each class 
#' @usage sampling_train_test(df, n_test = 5 )
#' 
#' @return A list of train and test dataframes
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' sampling_train_test(df, n_test = 5)
#' }
sampling_train_test <- function(df, n_test = 5 ){
  
  test <- dplyr::sample_n(df, n_test )
  #test_bkp <<- test
  ## omit test rows from train and remove duplicates if exist
  train <- df[!(rownames(df) %in% rownames(test)),]
  
  #train <-  df[-duplicated(rbind(test, df)),]
  
  #train_bkp <<- train
  return(list(train = train, test = test))
}

#' Merge training and testing datasets from a list of dataframes (classes)
#'
#' @param list.df list of train and test dataframes
#' @param n_test  number of test samples for each class
#' @usage merge_train_test(list.df, n_test)
#'
#' @return A list of merged classes of tain and test dataframes
#' @export
#'
#' @examples
#' \dontrun{
#' merge_train_test(list.df, 5)
#' }
#' @import mxnet
#' @importFrom keras fit evaluate predict_classes keras_model_sequential layer_dense layer_dropout compile to_categorical
merge_train_test <- function(list.df, n_test ){
  
  set.seed(1234)
  
  list_train_test <- lapply(list.df, sampling_train_test, n_test)
  
  sum_train <- data.frame()
  sum_test <- data.frame()
  for(i in 1:length(list_train_test)){
    
    sum_train <-rbind( sum_train, list_train_test[[i]]$train)
    
    sum_test <- rbind( sum_test, list_train_test[[i]]$test)
    
  }
  return(list(allTrain = sum_train, allTest = sum_test))
  
}