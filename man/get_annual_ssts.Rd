% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/average_ssts.R
\name{get_annual_ssts}
\alias{get_annual_ssts}
\title{Calculate the average temperature for each year, given a single year or a range of
years.}
\usage{
get_annual_ssts(hadsst_raster, years = 1969:2011)
}
\arguments{
\item{hadsst_raster}{the HadISST data as a raster object.}

\item{years}{a numeric vector specifying the years for which annual sea surface
temperatures will be calculated.}
}
\value{
a raster brick object with the average temperature (averages monthly values)
for each cell of each year specified.
}
\description{
Calculate the average temperature for each year, given a single year or a range of
years.
}
