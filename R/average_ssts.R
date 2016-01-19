#' Calculate the average temperature for each year, given a single year or a range of
#' years.
#'
#' @param hadsst_raster the HadISST data as a raster object.
#' @param years a numeric vector specifying the years for which annual sea surface
#' temperatures will be calculated.
#' @return a raster brick object with the average temperature (averages monthly values)
#' for each cell of each year specified.
#' @export
get_annual_ssts <- function(hadsst_raster, years = 1969:2011) {
	mean_rasts <-
		apply(matrix(years), 1, function(x) {
			#browser()
			yearIDx <- which(chron::years(hadsst_raster@z$Date) == x)
			subset_x <- raster::subset(hadsst_raster, yearIDx)
			means <- mean(subset_x, na.rm = TRUE)
			names(means) <- as.character(x)
			return(means)
		})
	#return(mean_rasts)
	mean_brick <- raster::brick(mean_rasts)
	mean_brick <- raster::setZ(mean_brick, as.Date(paste0(years, '-01-01')), 'Date')
	return(mean_brick)
}

#' Calculate the average temperature over a range of years
#'
#' @param hadsst_raster the HadISST data as a raster object.
#' @param years a numeric vector specifying the years used to calculate the
#' sea surface temperature.
#' @return a single-layer raster object with the average temperature for each cell of
#' the time period specified in years.
#' @export
get_average_sst <- function(hadsst_raster, years = 1969:2011) {
	yearIDs <- which(chron::years(hadsst_raster@z$Date) %in% years)
	subset_x <- raster::subset(hadsst_raster, yearIDs)
	average_sst <- mean(subset_x, na.rm = TRUE)
	return(average_sst)
}