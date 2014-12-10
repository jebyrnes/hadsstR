#' @title getSSTAnnualArray
#'
#' @description
#' \code{getSSTAnnualArray} Collapse monthly HadSST data into annual averages
#' 
#' @details Requires that you have loaded a HADSST data file.
#'
#' @author Jarrett Byrnes.
#' @param sstObj The HadSST Data object
#' @param years A range of years to use to examine climate change
#' 
#' 
#' @export
#' @return Returns an array with each slice containing one matrix of temperatures
#' averaged over a year
#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' summary(sstData)
#' 
#' getSSTAnnualArray(sstData, years=1984:2004)
#' 
getSSTAnnualArray <- function(sstObj, years=1969:2009){
  #Calculate an annual Average temperature map
  li <- length(sstObj$lat)
  lj <- length(sstObj$lon)
  sstAnnualArray <- array(NA, c(lj,li, length(years)))
  for(y in years){
    #which pieces of the SST array do we want?
    idx <- grep(y,sstObj$tdates)
    #extract a subset of the big data set
    yearSST <- sstObj$sstArray[,,idx]
    #average each lat/long per year and fill in the right place in b
    sstAnnualArray[,,grep(y,years)] <- apply(yearSST,c(1,2),function(x) mean(x, na.rm=T))
  }
  
  return(sstAnnualArray)
}