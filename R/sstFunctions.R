getSSTAvgMatFromArray <- function(sstAnnualArray){
  apply(sstAnnualArray, c(1,2), function(x) mean(x, na.rm=T))
}

getSSTAvgMat <- function(sstObj, years=1969:2009){
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  getSSTAvgMatFromArray(sstAnnualArray)
}

#decadal change
getSSTLinearChangeMatFromArray<- function(sstAnnualArray, years=1969:2009){
  changeMat <- apply(sstAnnualArray, c(1,2), function(x){
    #check if this is land
    if(sum(is.na(x))<length(x)) return(10*coef(lm(x~I(years-mean(years))))[2])
    return(NA)
  })
  
  changeMat

}


getSSTLinearChangeMat<- function(sstObj, years=1969:2009){
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  sstAnnualArray(sstAnnualArray, years)
}

getSSTChangeMat<- function(sstObj, years=1969:2009){
  sstAnnualArray <- getSSTAnnualArray(sstObj, years)
  changeMat <- sstAnnualArray[,,length(years)] - sstAnnualArray[,,1] 
  
  changeMat
  
}


getNSChangeMat <- function(averageMat){
  
  #make change matrices
  NSmat <- averageMat[1:(nrow(averageMat)-1),] - averageMat[2:nrow(averageMat),]
  NSmat <- rbind(NSmat, NA)
  NSmat <- NSmat/111.325 #correct for distance (km)
}


getEWChangeMat <- function(averageMat, latitudes){
 
  EWmat <- averageMat[,1:(ncol(averageMat)-1)] - averageMat[,2:ncol(averageMat)]
  EWmat <- cbind(EWmat, averageMat[,ncol(averageMat)] - averageMat[,1])
  EWmat <- t(t(EWmat)/111.325*cos(latitudes*pi/180))
}


#function to get the spatially averaged gradient
getSpatialGrad <- function(NSmat, EWmat, i,j){

  li <- ncol(NSmat)
  lj <- nrow(EWmat)
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
  xGrad <- weighted.mean(c(EWmat[i,j],EWmat[i,jl], 
                           EWmat[iu,jl], EWmat[iu,jr], EWmat[id,jl], EWmat[id,jr]),
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

getSpatialGradMatsFromMats <- function(NSmat, EWmat){
  #greate matrices for spatial gradients and velocity
  spatialMat <- matrix(NA, nrow=nrow(EWmat), ncol=ncol(EWmat))
  angleMat <- matrix(NA, nrow=nrow(EWmat), ncol=ncol(EWmat))
  
  for(i in 1:nrow(spatialMat)){
    for(j in 1:ncol(spatialMat)){
      spatialGrad <- getSpatialGrad(NSmat, EWmat, i,j)
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
  EWmat <- getEWChangeMat(averageMat, sstObj$lat)
  
  getSpatialGradMatsFromMats(NSmat, EWmat)
}

truncateMat <- function(mat, upper, lower){
  mat[mat>upper] <- upper
  mat[mat<lower] <- lower
  mat
}