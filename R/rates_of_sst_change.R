#' Calculate the decadal rate of temperature change over a given time period.
#'
#' @param hadsst_raster the HadISST data as a raster object.
#' @param years a numeric vector specifying the year(s) over which the decadal rate of
#' linear temperature change will be calculated.
#' @return a single layer raster with the decadal rate of linear temperature cell
#' for each cell of the time period specified in years
#' @export
get_sst_linear_change <- function(hadsst_raster = b, years = 2000:2011) {
	annual_rasters <- get_annual_ssts(hadsst_raster, years)

	time_ <- I(years - mean(years))
	fun <- function(x) {
		if (sum(is.na(x)) < length(x)) {
			slope <- 10 * lm(x ~ time_)$coefficients[2]
			return(slope)
		}
		return(NA)
	}
	lin_change <- raster::calc(annual_rasters, fun)
	return(lin_change)
}


#' Calculate the west to east spatial gradient in temperature between neighbouring
#' cells (corrected for distance by latitude).
#'
#' @param avg_raster is a single-layer raster containing the average sea surface
#' temperature over a range of years.
#' @return a single-layer raster with the west-east spatial gradient and
#' corrected for distance by latitude.
#' @seealso \code{hadsstr::get_average_sst}
#' @export
get_WE_diffs <- function(avg_raster) {
	f <- matrix(rep(1, 9), nrow=3, ncol=3)
	lat <- sp::coordinates(avg_raster)[, 2]
	WE_diff <- raster::focal(avg_raster, w = f, nrow=3, ncol=3, pad = TRUE,
																 fun = function(x, ...) {
																 	ba <- x[6] - x[5]
																 }
	)
	WE_adj_diff <- WE_diff / (111.325 * cos (lat * pi / 180))
	return(WE_adj_diff)
}

#' Calculate the north to south spatial gradient in temperature between
#' neighbouring cells.
#'
#' @param avg_raster is a single-layer raster containing the average sea surface
#' temperature over a range of years.
#' @return a single-layer raster with the north-south spatial gradient and
#' corrected for distance by latitude.
#' @seealso \code{\link{get_average_sst}}
#' @export

get_NS_diffs <- function(avg_raster) {
	f <- matrix(rep(1, 9), nrow=3, ncol=3)
	NS_diff <- raster::focal(avg_raster, w = f, nrow=3, ncol=3, pad = TRUE,
																 fun = function(x, ...) {
																 	be <- x[2] - x[5]
																 }
	)
	NS_adj_diff <- NS_diff / 111.325
	return(NS_adj_diff)
}

#' Calculate the spatial gradient of temperature change.
#'
#' @param NS_gradient single-layer raster containing the north to south
#' spatial gradient between neighbouring cells
#' @param WE_gradient single-layer raster containing the west to east
#' spatial gradient between neighbouring cells
#' @return a two-layer raster containing 'spatial_gradient', the magnitude of the spatial gradient and 'angle', the direction of the spatial gradient.
#' @seealso \code{hadsstr::get_average_sst}, \code{hadsstr::get_vocc}
#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' @export
get_spatial_gradient <- function(NS_gradient, WE_gradient) {
	f <- matrix(rep(1, 9), nrow=3, ncol=3)
	x_gradient <- raster::focal(WE_gradient, w = f, nrow=3, ncol=3, pad = TRUE,
												 fun = function(x, ...) {
												 	#browser()
												 	wt <- c(1, 2, 1, 2, 0, 2, 1, 2, 1)
												 	weighted.mean(x, wt, na.rm = TRUE)
												 })
	y_gradient <- raster::focal(NS_gradient, w = f, nrow = 3, ncol = 3, pad = TRUE,
												 fun = function(x, ...) {
												 	wt <- c(1, 2, 1, 2, 0, 2, 1, 2, 1)
												 	weighted.mean(x, wt, na.rm = TRUE)
												 })

	# Get magnitude of resultant vector
	magnitude <- sqrt(x_gradient^2 + y_gradient^2)

	# Get angle of resultant vector
	angle <- raster::atan2(x_gradient, y_gradient) * 180 / pi

	# Create correction raster to produce positive angles
	neg_angles <- (angle < 0) * 360

	angle <-
		raster::overlay(x = angle, y = neg_angles, fun = function(x, y) {x + y})

	spatial_grad_brick <- raster::brick(magnitude, angle)
	names(spatial_grad_brick) <- c('spatial_gradient', 'angle')
	return(spatial_grad_brick)
}

#' Calculate the velocity of climate change over a given set of years
#'
#' @param hadsst_raster the HadISST data as a raster object.
#' @param years a numeric vector specifying the years over which the velocity of
#' climate change will be calculated.
#' @param truncate logical expression indicating whether minimum and maximum
#' values should be constrained to -200 < x < 200. Default is TRUE.
#' @return a two-layer raster containing 'velocity_mag' and 'velocity_angle',
#' the magnitude and direction of the velocity of climate change, respectively.
#' @seealso \code{hadsstr::get_average_sst}
#' @references Loarie, S. R., P. B. Duffy, H. Hamilton, G. P. Asner, C. B. Field, and D. D. Ackerly. 2009. The velocity of climate change. Nature 462:1052–5.
#' Burrows, M. T., D. S. Schoeman, L. B. Buckley, P. Moore, E. S. Poloczanska, K. M. Brander, C. Brown, J. F. Bruno, C. M. Duarte, B. S. Halpern, J. Holding, C. V Kappel, W. Kiessling, M. I. O’Connor, J. M. Pandolfi, C. Parmesan, F. B. Schwing, W. J. Sydeman, and A. J. Richardson. 2011. The pace of shifting climate in marine and terrestrial ecosystems. Science (New York, N.Y.) 334:652–5.
#' @export
get_vocc <- function(hadsst_raster, years = 1969:2009, truncate = TRUE) {

	linear_change <- get_sst_linear_change(hadsst_raster, years)

	average_sst <- get_average_sst(hadsst_raster, years)

	WE_gradient <- get_WE_diffs(average_sst)
	NS_gradient <- get_NS_diffs(average_sst)

	spatial_gradient <- get_spatial_gradient(NS_gradient, WE_gradient)

	velocity <- linear_change / raster::subset(spatial_gradient, 'spatial_gradient')

	if (truncate == TRUE) {
		m <- c(-Inf, -200, -200, 200, Inf, 200)
		rclmat <- matrix(m, ncol = 3, byrow = TRUE)
		velocity <- raster::reclassify(velocity, m)
	}

	angle <- raster::subset(spatial_gradient, 'angle')

	velocity_brick <- raster::brick(velocity, angle)
	names(velocity_brick) <- c('velocity', 'angle')
	return(velocity_brick)
}


#' Calculate a raster brick object containing the average sst, decadal rate of
#' linear temperature change, spatial gradient, and velocity of climate change
#' over a time period specified by years.
#' @param hadsst_raster the HadISST data as a raster object.
#' @param years a numeric vector specifying the years over which metrics of
#' sea surface temperature will be calculated.
#' @return a multi-layer raster brick containing the average sea surface
#' temperature, decadal rate of linear temperature change, magnitude of the
#' spatial gradient, magnitude of the velocity of climate change, and the
#' angle of the spatial gradient and velocity of climate change (same angle).
#' @export
get_all_rasters <- function(hadsst_raster, years = 1969:2009) {
	average_sst <- get_average_ssts(hadsst_raster, years)

	linear_change <- get_sst_linear_change(hadsst_raster, years)
	WE_diffs <- get_WE_diffs(average_sst)
	NS_diffs <- get_NS_diffs(average_sst)

	spatial_gradient <- get_spatial_gradient(NS_diffs, WE_diffs)

	velocity <-
		linear_change / raster::subset(spatial_gradient, 'spatial_gradient')

	angle <- raster::subset(spatial_gradient, 'angle')

	all_rasters <- raster::brick(average_sst, linear_change,
															spatial_gradient, velocity, angle)

	names(all_rasters) <- c('average_sst', 'linear_change', 'spatial_gradient',
												 'velocity_magnitude', 'angle')
	return(all_rasters)
}

#' Select layers from the raster brick object created using \code{hadsstr::get_all_rasters}.
#' Layers contain the average sst, decadal rate of linear temperature change,
#' spatial gradient, and velocity of climate change for a set of years.
#' @param raster brick created using the \code{hadsstr::get_all_rasters} the HadISST data as a raster object.
#' @param layer_name character vector containing the name(s) of the layers
#' @return a raster or raster brick containing layers selected in 'layer_name'.
#' @export
select_raster <- function(all_rasters, layer_name) {
	selected_raster <- raster::subset(all_rasters, layer_name)
	return(selected_raster)
}

