#' Create my N curve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm
#'
#' @return the curve
#' @export
#'
#' @examples function(mu = 10, sigma = 10, a = 10)
myncurve = function(mu, sigma, a){
  curve(dnorm(x=a,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  area = pnorm(a,mean=mu,sd=sigma) - pnorm(mu-3*sigma,mean=mu,sd=sigma)
  area = round(area, 4)

  list(mu = mu, sigma = sigma, area = area)
}

