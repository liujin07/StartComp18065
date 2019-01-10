#' @title A function that generates random Numbers using the monte carlo method
#' @description  a function to compute a Monte Carlo estimate of the Beta(3, 3) cdf.
#'               and use the function to estimate F(x) for x = 0.1,0.2,…,0.9. Compare the estimates with the values returned by the pbeta function in R.
#' @param n The number of random Numbers generated
#' @param a The lower bound of the interval
#' @param b The upper bound of the interval
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' a <- data.frame(x=seq(0.1,0.9,0.1),Montacarlo=numeric(9),pbeta=numeric(9))#construct a dataframe to show the value of cdf
#' i <- 1
#' while (i<=9) {
#'   a[i,2] <- beta_cdf(10000,0,i*0.1)
#'   a[i,3] <- pbeta(i*0.1,3,3)
#'   i=i+1
#' }
#' print(a)
#' }
#' @export
beta_cdf <- function(n,a,b){
  x <- runif(n,a,b)
  cdf_estimate <- (sum(30*(x^2-2*x^3+x^4))/n)*(b-a)#Monta carlo methord
  return(cdf_estimate)
}

