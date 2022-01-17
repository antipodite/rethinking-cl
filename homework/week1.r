library(rethinking)

gridApprox <- function (n, m, obs, prior) {
  p_grid <- seq(from=0, to=1, length.out=n)
  prob_data <- dbinom(m, size=obs, prob=p_grid)
  posterior <- prob_data * prior
  posterior <- posterior / sum(posterior)
  samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
  return(samples)
}

# Globe tossing example with numbers from book:
prior <- rep(1, 1000) # Flat prior   
samples <-gridApprox(1000, 6, 9, prior)
plot(samples)
dens(samples)

# Homework exercise 1
# "Suppose the globe tossing data (Chapter 2) had turned out to be 4 water
#  and 11 land. Construct the posterior distribution, using grid approximation.
#  Use the same flat prior as in the book."
# The same calculation, but we tossed the globe 15 times and got 4 water and
# 11 land:
prior <- rep(1, 1000)
samples <- gridApprox(1000, 4, 15, prior)
plot(samples)
dens(samples)

# Homework exercise 2
# "Now suppose the data are 4 water and 2 land. Compute the posterior
#  again, but this time use a prior that is zero below p = 0.5 and a constant
#  above p = 0.5. This corresponds to prior information that a majority of the
#  Earth’s surface is water."
prior <- c(rep(0, 500), rep(7, 500))
samples <- gridApprox(1000, 4, 6, prior)
plot(samples)
dens(samples)

# Homework exercise 3
# "For the posterior distribution from 2, compute 89% percentile and HPDI
#  intervals. Compare the widths of these intervals. Which is wider? Why? If
#  you had only the information in the interval, what might you misunderstand
#  about the shape of the posterior distribution?"
hdpi <- rethinking::HPDI(samples)
percentile <- rethinking::PI(samples, prob=)