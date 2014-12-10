#' @title truncateMat
#'
#' @description
#' \code{truncateMat} Convenience function to truncate extreme values in a matrix
#' 
#' @details This is ideal for plotting matrices with extreme high and low values
#'
#' @author Jarrett Byrnes.
#' @param mat The matrix to be turncated
#' @param upper The upper limit
#' @param lower lower limit

#' 
#' @export
#' @return Returns a \code{matrix} 
#'
#' @examples
#' mat <- matrix(1:100, nrow=10)
#' truncateMat(mat, 80, 20)
#' 
truncateMat <- function(mat, upper, lower){
  mat[mat>upper] <- upper
  mat[mat<lower] <- lower
  mat
}