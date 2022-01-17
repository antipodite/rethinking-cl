library(rethinking)

ex2.6 <- function () {
  globe.qa <- quap(
    alist(
      W ~ dbinom( W+L ,p) , # binomial likelihood
      p ~ dunif(0,1)
      # uniform prior
    ) ,
    data=list(W=6,L=3) )
  # display summary of quadratic approximation
  precis( globe.qa )
  return( globe.qa )
}
 
