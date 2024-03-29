% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries.R
\name{get_matches}
\alias{get_matches}
\title{Use the regex match groups and the time series to compile two lists: matching indices, and matching entries.}
\usage{
get_matches(ts, m, exact)
}
\arguments{
\item{ts}{Time series  list , Time series data}

\item{m}{Regex match groups  list , The query string split according to the regex matches.}

\item{exact}{Key match  char. Is the provided key an exact key match or a piece of the key? ie. paleoData_variableName or variableName?}
}
\description{
Use the regex match groups and the time series to compile two lists: matching indices, and matching entries.
}
\author{
Chris Heiser
}
\keyword{internal}
