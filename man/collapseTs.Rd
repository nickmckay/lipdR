% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries_collapse.R
\name{collapseTs}
\alias{collapseTs}
\title{Collapse time series into LiPD dataset form}
\usage{
collapseTs(ts, force = FALSE)
}
\arguments{
\item{ts}{Time series : list}

\item{force}{Attempt to collapse time series when lipd ts_storage is not provided: bool}
}
\value{
D: LiPD data, sorted by dataset name : list
}
\description{
Collapse time series into LiPD dataset form
}
\examples{
\dontrun{
D <- readLipd()
ts <- extractTs(D)
D <- collapseTs(ts)
}

}
\author{
Chris Heiser
}
