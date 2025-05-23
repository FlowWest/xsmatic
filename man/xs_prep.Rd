% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xs.R
\name{xs_prep}
\alias{xs_prep}
\title{Prep cross section data}
\usage{
xs_prep(data, sta, elev, delta_x = 0.1)
}
\arguments{
\item{data}{A tibble or data frame containing station-elevation pairs}

\item{sta}{The name of the station variable in the source data}

\item{elev}{The name of the elevation variable in the source data}

\item{delta_x}{The x interval width to be used for densification. Narrower intervals will produce better results. Defaults to 0.1 ft.}
}
\description{
Input a table of station-elevation cross section coordinates, such as from a rod-and-level survey.
Output a data frame with densified coordinates that can be used for post-processing.
}
