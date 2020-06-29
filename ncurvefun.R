#' normal curve function
#'
#' @param mu = mean
#' @param sigma = standard deviation
#' @param a = quantile
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma, a ){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(-1000,a ,length=1000)
  ycurve = dnorm(xcurve, mu, sigma)


  polygon(c(x = -1000,xcurve,x = a),c(0,ycurve,0),col="Blue")

  A = pnorm(a,mu,sigma,)

  A = round(A,4)

  list(A)
}
myncurve(mu = 3, sigma = 2, a = 2.5 )


