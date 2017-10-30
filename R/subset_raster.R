#' Subset the hadsst raster by year.
#'
#' @param hadsst_raster the HadISST data as a raster object.
#' @param years a numeric vector specifying the year(s) over which the decadal rate of
#' linear temperature change will be calculated.
#' @param brick a boolean field specifying if you would like to return the subsetted
#' raster as a list of rasters or as a single brick of rasters.
#' @return a single layer raster with the decadal rate of linear temperature cell
#' for each cell of the time period specified in years
#' @export

subset_years <- function(hadsst_raster, years = 2000:2010, brick = TRUE) {
    yearIDx <- which(chron::years(hadsst_raster@z$Date) %in% years)
    subset_x <- raster::subset(hadsst_raster, yearIDx)
    if (brick == TRUE) {
      return(raster::brick(subset_x))
    } else {
      return(subset_x)
  }
}