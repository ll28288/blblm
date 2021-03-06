% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{predict.blblm}
\alias{predict.blblm}
\title{prediction}
\usage{
\method{predict}{blblm}(object, new_data, confidence = FALSE, level = 0.95, ...)
}
\arguments{
\item{object}{blblm model}

\item{new_data}{new data to predict}

\item{confidence}{logic value if we want an interval}

\item{level}{confidence level}

\item{...}{...}
}
\value{
vector of interval
}
\description{
predict value base on our model
}
