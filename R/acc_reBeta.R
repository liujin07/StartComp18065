#' @title The function that generates random Numbers with the accept-rejection method
#' @description a function to generate a random sample of size n from the Beta(a,b) distribution by the acceptance-rejection method.
#' @param n The number of random Numbers generated
#' @param a Parameter of beta distribution
#' @param b Parameter of beta distribution
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' sample_beta <- betafunction(1e3,3,2)#record the sample
#' head(sample_beta,20)
#' hist(sample_beta)
#' }
#' @export
betafunction <- function(n,a,b){
  #bata_pdf<-function{
  #  (1/beta(a,b))*x^(a-1)* (1-x)^(b-1)
  #}
  j<-k<-0;y <- numeric(n)#
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1) #random variate from g
    #if (x * (1-x) > u)
    if (x^(a-1)* (1-x)^(b-1) > u) {
      #we accept x
      k <- k + 1
      y[k] <- x
    }
  }
  return(y)
}
