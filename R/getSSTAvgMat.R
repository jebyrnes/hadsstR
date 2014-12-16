#' @title getSSTAvgMat
#'
#' @description
#' \code{getSSTAvgMat} Gets the matrix describing average temperature across years
#' 
#' @details Requires that you have loaded a HADSST data file. Will return a matrix of average temperature
#' of all lat/longs in all selected years
#'
#' @author Jarrett Byrnes.
#' @param sstAnnualArray An array of SST values at different points in time
#' @param years A range of years to use to examine climate change
#' 
#' @export
#' @return Returns a matrix of average temperature values
#' 
#' @seealso \code{\link{getSSTLinearChangeMatFromArray}}, \code{\link{getSSTAvgMatFromArray}}
#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' 
#' getSSTAvgMat(sstData, years=1969:2009)
#' 
getSSTAvgMat <- function(sstObj, years=1969:2009){
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  getSSTAvgMatFromArray(sstAnnualArray)
}