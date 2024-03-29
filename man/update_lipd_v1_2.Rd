% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/versions.R
\name{update_lipd_v1_2}
\alias{update_lipd_v1_2}
\title{Update LiPD v1.1 to v1.2
- Added NOAA compatible keys : maxYear, minYear, originalDataURL, WDCPaleoURL, etc
- 'calibratedAges' key is now 'distribution' (handled in update_lipd_v1_3_keys instead)
- paleoData structure mirrors chronData. Allows measurement, model, summary, ensemble,
  distribution tables}
\usage{
update_lipd_v1_2(d)
}
\arguments{
\item{d}{Metadata}
}
\value{
list d: Metadata
}
\description{
Update LiPD v1.1 to v1.2
- Added NOAA compatible keys : maxYear, minYear, originalDataURL, WDCPaleoURL, etc
- 'calibratedAges' key is now 'distribution' (handled in update_lipd_v1_3_keys instead)
- paleoData structure mirrors chronData. Allows measurement, model, summary, ensemble,
  distribution tables
}
\keyword{internal}
