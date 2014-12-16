#' @title getSSTLinearChangeMatFromArray
#'
#' @description
#' \code{getSSTLinearChangeMatFromArray} Gets the matrix with linear change coefficients for 
#' temperature in each grid cell
#' 
#' @details Requires that you have loaded a HADSST data file. Will return a matrix of decadal slopes
#' for all lat/longs in all selected years. Note, for regressions below the annual level, the 'years'
#' argument must be in decimal years.
#'
#' @author Jarrett Byrnes.
#' @param sstAnnualArray The HadSST Data object
#' @param years The range of time in years for the array being used.
#' 
#' @export
#' @return Returns a matrix of average temperature values
#' 
#' @seealso \code{\link{getClimateChange}}, \code{\link{getSSTAnnualArray}}
#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' 
#' years <- 1960:2009
#'
#' sstAnnualArray <- getSSTAnnualArray(sstData, years)
#'
#' getSSTLinearChangeMatFromArray(sstData$sstAnnualArray, years)
#' 
#decadal change
getSSTLinearChangeMatFromArray<- function(sstAnnualArray, years=1969:2009){
  changeMat <- apply(sstAnnualArray, c(1,2), function(x){
    #check if this is land
    if(sum(is.na(x))<length(x)) return(10*coef(lm(x~I(years-mean(years))))[2])
    return(NA)
  })
  
  changeMat
  
}
