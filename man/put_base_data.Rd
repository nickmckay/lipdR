% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries_collapse.R
\name{put_base_data}
\alias{put_base_data}
\title{Put in paleoData and chronData as the base for this dataset using the oroginal dataset data.}
\usage{
put_base_data(entry, raw_datasets, dsn, force, mode)
}
\arguments{
\item{entry}{ts entry}

\item{raw_datasets}{stored loaded data}

\item{dsn}{datasetname}

\item{force}{Build dataset without original data from lipd}

\item{mode}{paleo or chron mode}
}
\value{
d: Metadata
}
\description{
Put in paleoData and chronData as the base for this dataset using the oroginal dataset data.
}
