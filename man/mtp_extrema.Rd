% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/min_max_extreme.R
\name{mtp_extrema}
\alias{mtp_extrema}
\title{The value and location of curve extremas within the search window}
\usage{
mtp_extrema(mtp, start = NULL, end = NULL)
}
\arguments{
\item{mtp}{An \code{mtp} object}

\item{start}{a \code{scalar} defining the start of the search}

\item{end}{a \code{scalar} defining the end of the search}
}
\value{
Returns a data frame of features
}
\description{
The value and location of curve extremas within the search window
}
\examples{
extrema <- df_mtp$mtp[[1]]
mtp_extrema(extrema, start = 2, end = 10)extrema
}

