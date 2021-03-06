% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Samp_TrainTest.R
\name{sampling_train_test}
\alias{sampling_train_test}
\title{Sampling training and testing row from dataframe}
\usage{
sampling_train_test(df, n_test = 5 )
}
\arguments{
\item{df}{dataframe with samples in the row and pixels in columns.
The first column is the label or class}

\item{n_test}{number of test samples for each class}
}
\value{
A list of train and test dataframes
}
\description{
Sampling training and testing row from dataframe
}
\examples{
\dontrun{

sampling_train_test(df, n_test = 5)
}
}
