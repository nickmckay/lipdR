% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changelog.R
\name{updateChangelog}
\alias{updateChangelog}
\title{Update the changelog entry in a LiPD file with new changes}
\usage{
updateChangelog(
  L,
  changelog,
  version = NA,
  notes = NA,
  curator = Sys.info()[["user"]],
  timestamp = lubridate::now(tzone = "UTC")
)
}
\arguments{
\item{L}{a LiPD object}

\item{changelog}{a changelog data.frame}

\item{version}{version of the dataset}

\item{notes}{notes}

\item{curator}{who made these changes}

\item{timestamp}{when was this made}
}
\value{
An updated LiPD object
}
\description{
Update the changelog entry in a LiPD file with new changes
}
