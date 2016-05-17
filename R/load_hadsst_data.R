#' Load the netcdf sea surface temperature data.
#'
#' @name load_hadsst
#' @param path to HadISST data in the netcdf format
#' @export
load_hadsst <- function(file = "./HadISST_sst.nc") {
	b <- raster::brick(file)
	raster::NAvalue(b) <- -1000
	return(b)
}
