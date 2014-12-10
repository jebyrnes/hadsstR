#' @title getClimateChange
#'
#' @description
#' \code{getClimateChange} Takes a HadSST dataset and produces a variety of matrices
#' 
#' @details Requires that you have loaded a HADSST data file.
#'
#' @author Jarrett Byrnes.
#' @param sstObj The HadSST Data object
#' @param years A range of years to use to examine climate change

#' 
#' @export
#' @return Returns a \code{hadsstMats} object containing an variety of
#' matrices summarizing climate and climate change as well as information
#' about latitude, longitude, and the years examined
#' 
#' @return \code{averageMat} The average temperature over the timespan
#' @return \code{linearChangeMat} The linear change in temperature over
#' the timespan defined in degrees/decade calculate by regressing annual 
#' average temperature against years and multiplying by 10
#' @return \code{velocityMat} The velocity of climate change defined as the
#' linear change divided by the spatial gradient in temperature in units of
#' degrees C/decade/degree latitude
#' @return \code{angleMat} The angle of climate change
#' @return \code{spatialGradMat} The spatial gradient in temperature for each 
#' cell in degrees C per degree latitude
#' @return \code{NSmat} The N-S gradient in temperature for each cell
#' @return \code{EWmat} The E-W gradient in temperature for each cell
#' @return \code{lat} Latitudes of the matrices
#' @return \code{lon} Longitudes of the matrices
#' @return \code{years} The years assessed
#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' 
#' climateChangeMats <- getClimateChange(sstData)
#' 
#' #Let's plot some of these matrices
#' pal <- colorRampPalette(c("blue","white", "red"))
#' pal2 <- colorRampPalette(c("green", "lightblue", "white", "yellow", "orange"))
#' #' #create a lat/long grid for easier plotting
#' library(lattice)
#' latLonGrid <- expand.grid(lon = climateChangeMats$lon, lat = climateChangeMats$lat)
#' 
#' with(climateChangeMats, image(lon, lat, averageMat, col=pal(80)))
#' 
#' levelplot(climateChangeMats$averageMat ~ lon * lat, data = latLonGrid, col.regions = pal(101))
#'
#' levelplot(climateChangeMats$linearChangeMat ~ lon * lat, 
#'  data = latLonGrid, col.regions = pal(101), at=seq(-1,1,length.out=101))
#'
#' with(climateChangeMats, image(lon, lat, spatialGradMat, col=pal(81)))
#' with(climateChangeMats, image(lon, lat, NSmat, col=pal2(80)))
#' levelplot(climateChangeMats$EWmat ~ lon * lat, col.regions=pal2(100),
#'  data = latLonGrid, at=seq(-0.025, 0.025, length.out=100))
#' 
#' #create a velocity matrix where values >200 and < -200 are truncated to those limits
#'#for easier plotting, as in Burrows et al. 20011
#' velMatTruncated <- truncateMat(climateChangeMats$velocityMat, 200, -200)
#' levelplot(velMatTruncated ~ lon * lat, data = latLonGrid, #at = cutpts, 
#'           pretty = T, 
#'          col.regions = pal(100),
#'           at=seq(-200,200,length.out=100))
#' 
#' 
getClimateChange <- function(sstObj, years=1969:2009){
  #get the array of temps over all years, averaged by year
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  
  #get the average matrix of temps
  averageMat <-getSSTAvgMatFromArray(sstAnnualArray)
  
  #get the linear change matrix
  linearChangeMat <-getSSTLinearChangeMatFromArray(sstAnnualArray, years)
  
  #get info on spatial gradients
  NSmat <- getNSChangeMat(averageMat)
  EWmat <- getEWChangeMat(averageMat, sstObj$lat)
  
  #greate matrices for spatial gradients and velocity
  spatialMats <-   getSpatialGradMatsFromMats(NSmat, EWmat)
  
  velocityMat <- linearChangeMat/spatialMats$spatialGradMat
  
  ret <- list(averageMat=averageMat, linearChangeMat=linearChangeMat,
              NSmat=NSmat,EWmat=EWmat, spatialGradMat=spatialMats$spatialGradMat,
              angleMat = spatialMats$angleMat, velocityMat = velocityMat,
              lat=sstObj$lat, lon=sstObj$lon, years=years)
  
  class(ret) <- "hadsstMats"
  
  return(ret)
}