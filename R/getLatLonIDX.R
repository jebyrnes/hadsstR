#' @title getLatLonIDX
#'
#' @description
#' \code{getLatLonIDX} Gets the matrix indecies of a given latitude and longitude
#' 
#' @details Requires that you have loaded a HADSST data file and/or made a set of climate change matrices.
#' As lat/long informaiton in the data file is centered around 0.5 for each cell, if the lat or long 
#' being searched for is a round number (e.g., 3), the function will return values from the cell with the 
#' lower number (e.g., 2.5 instead of 3.5)
#'
#' @author Jarrett Byrnes.
#' @param lat Latitude
#' @param lon Longitude
#' @param obj An object returned by \code{getClimateChange} or  \code{loadHadSST}

#' 
#' @export
#' @return Returns a vector with a row and column index for a matrix derived from a netcdf file
#' 

#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' 
#' getLatLonIDX(-50.232, -100.55, sstData)
#' 
#' 


getLatLonIDX <- function(lat, lon, obj){
  rIDX  <- which(abs(round(lon - obj$lon))==0)[1]
  cIDX <- which(abs(round(lat - obj$lat))==0)[1]
  
  return(c(rIDX, cIDX))
}