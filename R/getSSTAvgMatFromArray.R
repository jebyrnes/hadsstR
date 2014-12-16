#' @title getSSTAvgMatFromArray
#'
#' @description
#' \code{getSSTAvgMatFromArray} Gets the matrix describing average temperature across years 
#' from an array of geospatial temperature values
#' 
#' @details Requires that you have loaded a HADSST data file. Will return a matrix of average temperature
#' of all values in the third dimension of the array
#'
#' @author Jarrett Byrnes.
#' @param sstAnnualArray An array of SST values at different points in time
#' 
#' @export
#' @return Returns a matrix of average temperature values
#' 
#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' 
#' getSSTAvgMatFromArray(sstData$sstArray)
#' 
getSSTAvgMatFromArray <- function(sstAnnualArray){
  apply(sstAnnualArray, c(1,2), function(x) mean(x, na.rm=T))
}