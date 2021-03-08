#' My normal curve
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The area of the distribution under X≤a will be shaded
#'
#' @return A plot of the normal distribution with with the area under X≤a shaded; the area
#' @export
#'
#' @examples myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){

  # Generate normal curve with given parameters
  curve(dnorm(x,mean=mu,sd=sigma),xlim=c(mu-3*sigma,mu+3*sigma))

  # Set min as lower limit of viewing window
  min = mu-3*sigma

  # Set max as 'a'
  max = a

  # x values corresponding to the x-cords of points on the curve
  xcurve=seq(min,max,length=1000)

  # Y values corresponding to the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(min,xcurve,max),c(0,ycurve,0),col="Red")

  # Find area
  prob=pnorm(max,mean=mu,sd=sigma)
  prob=round(prob,4)
  prob
}
