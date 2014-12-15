#' @title getClimateChange
#'
#' @description
#' \code{getClimateLatLon} Get information about climate change at a particular latitude and longitude
#' 
#' @details Requires that you have generated climate change matrices
#'
#' @author Jarrett Byrnes.
#' @param sstMatsObj An object containing latitude, longitude, and matrices of climate information from \code{climateChangeMats}
#' @param lat Latitude
#' @param lon Longitude
#' @param measure A string defining which type of information. Defaults to "velocity". Can be "average", "spatialGrad", 
#' "angle", "velocity", "NS", "EW". 
#' @export
#' @return Returns a single value
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' 
#' climateChangeMats <- getClimateChange(sstData)
#' 
#' getClimateLatLon(climateChangeMats, -50.232, -100.55, "linearChange")
#' 
#' 

getClimateLatLon <- function(sstMatsObj, lat, lon, measure="velocity"){
  measure <- paste0(measure, "Mat")
  idx <- getLatLonIDX(lat, lon, sstMatsObj)
  sstMatsObj[[measure]][idx[1],idx[2]]
  
}
