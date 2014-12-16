#' @title loadHadSST
#'
#' @description
#' \code{loadHadSST} Loads data from a HADSST netcdf file and returns a hadsstDATA object
#' 
#' @details Requires that you have downloaded and unpacked a HADSST data file.
#'
#' @author Jarrett Byrnes.
#' @param directory Where is the SST netcdf file found?
#' @param hadsstFilename The name of the netcdf file
#' @param latFlip Some of the HADSST data files have the sign of the latitude flipped.
#' Defaults to \code{TRUE}, and flips the array by latitude
#' 
#' 
#' 
#' @export
#' @return Returns a \code{hadsstData} object containing an array of measurements, the latitudes
#' and longitudes of data as well as the dates
#' @return \code{sstArray} The array of SST by longitude, latitude, and time
#' @return \code{lon} The values of longitude
#' @return \code{lat} The values of latitude
#' @return \code{tdates} The dates of each slice of the data
#'
#' @examples
#' sstData <- loadHadSST(directory="./", hadsstFilename="HadISST_sst.nc") 
#' summary(sstData)
#' 
loadHadSST <- function(directory="./", hadsstFilename="HadISST_sst.nc", latFlip=TRUE){
  require(ncdf)
  require(chron)
  
  #open the netcdf file
  sst.nc <- open.ncdf(paste0(directory, hadsstFilename))
  
  
  # define the bounding box for the raster
  #note that latitude got flipped somehow...
  lat <- sort(get.var.ncdf(sst.nc, "latitude", verbose = F))
  lon <- get.var.ncdf(sst.nc, "longitude", verbose = F)
  latLonGrid <- expand.grid(lon = lon, lat = lat)
  
  #how big of a grid do we have?
  li <- length(lat)
  lj <- length(lon)
  
  
  #get info on dates
  t <- get.var.ncdf(sst.nc, "time")
  tunits <- att.get.ncdf(sst.nc, "time", "units")
  
  # split the time units string into fields
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth = as.integer(unlist(tdstr)[2])
  tday = as.integer(unlist(tdstr)[3])
  tyear = as.integer(unlist(tdstr)[1])
  tdates <- as.Date(chron(t, origin = c(tmonth, tday, tyear)))
  tlen <- length(t)
  
  

  # get all values
  sst <- get.var.ncdf(sst.nc, varid="sst")
  n <- sst[1] # RNetCDF gets the NA value right without this
  sst[sst==n] <- NA  
  
  
  # SST values in array [lon,lat,month]
  sstArray <- array(get.var.ncdf(sst.nc, varid="sst"), c(lj,li,tlen))
  n <- sstArray[1] # RNetCDF gets the NA value right without this
  sstArray[sstArray==n] <- NA  
  dimnames(sstArray) <- list(lon, lat, as.character(tdates))
  
  
  #Something is weird with latitude - it appears flipped
  #so - flip it in the array
  if(latFlip) sstArray[,1:li,] <- sstArray[,li:1,]
  
  ret <- list(sstArray=sstArray, lat=lat, lon=lon, tdates=tdates)
  
  class(ret) <- "hadsstData"
  
  #cleanup
  close.ncdf(sst.nc)
  
  return(ret)
}

summary.hadsstData <- function(sstObj){
  cat("Latitude Range:", range(sstObj$lat))
  cat("\nLatitude Scale:", (sstObj$lat[2]-sstObj$lat[1]), " degree")
  cat("\nLongitude Range:", range(sstObj$lon))
  cat("\nLongitude Scale:", (sstObj$lon[2]-sstObj$lon[1]), " degree\n")
  cat("\nDate Range:", as.character(sstObj$tdates[1]), 
      as.character(sstObj$tdates[length(sstObj$tdates)]))
  cat("\nDate Scale: ", sstObj$tdates[6]-sstObj$tdates[5], " days")
}