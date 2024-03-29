% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/42_logisitcal_learning.R
\name{get_data_for_prediction}
\alias{get_data_for_prediction}
\title{Transforms data set into a format suitable for logit regressions.}
\usage{
get_data_for_prediction(data_set, model)
}
\arguments{
\item{data_set}{a data set as \code{data.frame} or \code{tibble}}

\item{model}{a list with the following items
\itemize{
\item `dependent` the name of the dependent variable as a character string
\item `regressors` the names of the regressor variables as a character vector
\item `experience` the experience levels as a two-dimensional character
  of which vector with values of either
  \itemize{
    \item Uerfaren
    \item Grunnleggende
    \item Videregende
    \item Avansert
  }
}}
}
\value{
a data set (as a \code{data.frame}) shortened and thus suitable for
  prediction via logit regressions; the output data has dependent variable
  and regressors only
}
\description{
The transformation on the input \code{data_set} are:
  \itemize{
    \item compute a short data set that only has dependent and regressor vars
    \item transform these vars into \code{factor}s
    \item re-code, the \code{leder_c} into "Nei"/"Ja", if the variable is
    used
    \item reduce short data set to a comparison between experience levels
    (which is 0/1) as defined via argument \code{experience}; e.g. if
    \code{experience = c("Uerfaren", "Avansert")} then 'Uerfaren'=0
    and 'Avansert'=1 which are then the possible realizations of the
    dependent variable
  }
}
