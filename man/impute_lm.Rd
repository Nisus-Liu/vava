% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vava_funset.R
\name{impute_lm}
\alias{impute_lm}
\title{imputed by regress}
\usage{
impute_lm(data)
}
\arguments{
\item{data}{numeric \code{data.frame}, that first column of data frame is dependent variable.}
}
\description{
substitute \code{NA} by regression values
}
\details{
Make sure that first column of data frame is dependent variable: Y, because progress will 
regards first column as dependent variable anyway.
}
