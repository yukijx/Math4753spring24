#' MyCLT function that creates a sample and a graph to represent it
#'
#' @param n size
#' @param iter number of iterations
#' @param a lower limit
#' @param b uppper limit
#' @importFrom stats runif
#' @importFrom graphics hist
#'
#' @return histogram and a curve of the normal
#' @export
#'
#' @examples myclt(n=10,iter=10000)
#'
myclt=function(n,iter,a=0,b=5){

  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)

  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  sm
}
