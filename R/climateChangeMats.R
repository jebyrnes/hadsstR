#' Climate Change matrices for 1960-2009
#'
#' A hadsstMats object for processed data from 1960-2009
#'
#' @format A  \code{hadsstMats} object with a variety of matrices
#' \describe{
#'   See \code{\link{getClimateChange}} for details of parts of object
#' }
#' 
#' 
#' @examples 
#' data(climateChangeMats)
#' 
#' pal <- colorRampPalette(c("blue","white", "red"))
#' library(lattice)
#' latLonGrid <- expand.grid(lon = climateChangeMats$lon, lat = climateChangeMats$lat)
#' 
#' with(climateChangeMats, image(lon, lat, averageMat, col=pal(80)))
#' 
#' @source \url{http://github.com/jebyrnes/hadsstR}
"climateChangeMats"