getSSTChangeMat<- function(sstObj, years=1969:2009){
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  changeMat <- sstAnnualArray[,,length(years)] - sstAnnualArray[,,1] 
  
  changeMat
  
}

#note - you might think that I've reversed rows and columns in the below method
#but, the matrices are stored on their sides - so this is a little wonky
#e.g. I use rows for WE and columns for NS due to being a transposed matrix
getWEChangeMat <- function(averageMat,latitudes){
  
  #make change matrices
  WEmat <- averageMat[1:(nrow(averageMat)-1),] - averageMat[2:nrow(averageMat),]
  WEmat <- rbind(WEmat, averageMat[nrow(averageMat),] - averageMat[1,])
  WEmat <- WEmat / (111.325 * cos(latitudes*pi/180))
  WEmat * -1 #multiplying by -1 so that it is compatible with Burrows, higher temp in the East
}



getNSChangeMat <- function(averageMat){

  NSmat <- averageMat[,2:ncol(averageMat)] - averageMat[,1:(ncol(averageMat)-1)]
  NSmat <- cbind(NSmat, NA)
  NSmat <- NSmat/111.325
  NSmat 
}


#function to get the spatiall? averaged gradient
getSpatialGrad <- function(NSmat, WEmat, i,j){

  li <- ncol(NSmat)
  lj <- nrow(WEmat)
  # print(c(i,j))
  #get bounding box indices
  id <- i+1
  iu <- i-1
  jl <- j-1
  jr <- j+1
  if(jr>li) jr<-1 #wrap
  if(jl==0) jl<-li #wrap
  if(id>lj) return(c(NA, NA)) #meh, it's ice
  if(iu==0) return(c(NA, NA)) #meh, it's ice
  
  
  yGrad <- weighted.mean(c(NSmat[i,j],NSmat[iu,j], 
                           NSmat[iu,jl], NSmat[iu,jr], NSmat[id,jl], NSmat[id,jr]),
                         c(2,2,rep(1,4)), na.rm=T)
  #oops - did this the wrong direction, so, multiplying by -1 to correct
  xGrad <- weighted.mean(c(WEmat[i,j],WEmat[i,jl], 
                           WEmat[iu,jl], WEmat[iu,jr], WEmat[id,jl], WEmat[id,jr]),
                         c(2,2,rep(1,4)), na.rm=T)
  
  #some convrsion to radial coordinates
  vecSum <- sqrt(xGrad^2+yGrad^2)
  vecAngle <- NA
  if(!is.na(vecSum)){
    vecAngle <- 90-atan2(yGrad, xGrad)*180/pi
    if(vecAngle<0) vecAngle <- 360+vecAngle
  }
  
  return(c(vecSum, vecAngle))
  
}

getSpatialGradMatsFromMats <- function(NSmat, WEmat){
  #greate matrices for spatial gradients and velocity
  spatialMat <- matrix(NA, nrow=nrow(WEmat), ncol=ncol(WEmat))
  angleMat <- matrix(NA, nrow=nrow(WEmat), ncol=ncol(WEmat))
  
  for(i in 1:nrow(spatialMat)){
    for(j in 1:ncol(spatialMat)){
      spatialGrad <- getSpatialGrad(NSmat, WEmat, i,j)
      spatialMat[i,j] <- spatialGrad[1]
      angleMat[i,j] <- spatialGrad[2]
    }
  }
  
  return(list(spatialGradMat = spatialMat, angleMat = angleMat))
}

getSpatialGradMats <- function(sstObj, years=1969:2009){
  #get the array of temps over all years, averaged by year
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  
  #get the average matrix of temps
  averageMat <-getSSTAvgMatFromArray(sstAnnualArray)
  
  #get info on spatial gradients
  NSmat <- getNSChangeMat(averageMat)
  WEmat <- getWEChangeMat(averageMat, sstObj$lat)
  
  getSpatialGradMatsFromMats(NSmat, WEmat)
}
