library(rethinking)
# Rethinking Linear Regression chapter examples and exercises

# Chapter introduces linear regression as a Bayesian procedure
# AFAIK linear regression is 2 variables?
# Remember the football field example for why normal distributions are normal - 
# because they sum the outcomes of stochastic processes / fluctuations from the 
# mean and large deviations from the mean are always going to be rarer than smaller
# ones.
# Can get normal distributions by addition or multiplication

# Football field experiment (addition)
pos <- replicate(1000, sum(runif(16, -1, 1))) 

# Growth rate experiment (multiplication)
prod( 1 + runif(12,0,0.1) )
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

# Also, "large deviates that are multiplied together do not produce Gaussian
# distributions, but they do tend to produce Gaussian distributions on the log
# scale (remember adding logs = multiplication nonlogs)
log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )

# Ontological (empirical) justification for using normals: nature is full of them
# Epistemological (a priori) jusitification : "represents a particular state of
# ignorance." The Gaussian distribution is most consistent with our assumptions
# when all we are willing to say about our set of measurements in their mean and
# variance -- "the Gaussian distribution is the shape that can be realised in the
# largest number of ways and does not introduce any new assumptions"

# Language of models:
# - DATA : observed variables
# - PARAMETERS : unobservable variables
# - Each variable is defined in terms of other variables or as a PROBABILITY DISTRIBUTION
# - The combination of the variables and their probability distributions defines a JOINT
#   GENERATIVE MODEL that can be used to both simulate hypothetical observations as well as 
#   analyse real ones
# - "...talk about models as mappings of one set of variables through a probability disttribution
#   to another set of variables."

#
# Gaussian model of height
# hi ~ Normal(μ, σ) : each observation of height is on a normal distribution with mean μ and std dev σ [likelihood]
# We're estimating both the mean and stddev parameters so we need a joint prior for these.
# You specify priors seprately for each parameter and then the joint prior is the product Pr(μ)Pr(σ)
# μ ~ Normal(178, 20) : the prior for the mean is normally distributed centered on 178 with stddev of 20
#                     : 178 is a plausible prior for the mean of a human pop height if you think about it,
#                     : so we've used scientific knowledge to arrive at this prior
# σ ~ Uniform(0, 50) : the prior for the stddev is uniform somewhere between 0 and 50. All this
#                    : does is constrain sigma to have +ve probability between 0 and 50

data("Howell1")
df <- Howell1
precis(df)

adults <- df[ df$age >= 18 , ]

# Plot the priors:
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

# Simulate heights by sampling from the prior (the 'r' prefix distribution functions just give
# you random deviates):
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens(prior_h)
# Prior predictive simulation like this is useful for showing you how the priors influence
# the observable variables. Compare with a less informative prior with a wider stddev:
sample_mu_stupid <- rnorm( 1e4 , 178 , 100 )
prior_h_stupid <- rnorm( 1e4 , sample_mu_stupid , sample_sigma )
dens(prior_h_stupid) # See how now it's telling you the mean is 2 metres tall and some people
                     # have negative height. Be careful g

# Computing approximation of the posterior distribution with QUADRATIC APPROXIMATION:
heightmodel <- alist( # notice you're referencing vars before assignment, this is what an alist lets you do
  height ~ dnorm(mu, sigma),
  mu     ~ dnorm(178, 20),
  sigma  ~ dunif(0, 50)
)
posterior.approx <- quap(heightmodel, data=adults)
precis(posterior) # Gaussian approximations for each parameter's marginal distribution, i.e
                  # the plausibility of each value of mu after averaging over the plausibilities
                  # of each value of sigma is given by a normal dist with mean 154.6 and sd 0.4
                  # MARGINAL - the probability distribution of the parameters

# Now how do you get samples from the quadratically approximated posterior?
# "The answer is rather simple but non-obvious and it requires recognising that a quadratic
# approximation to a posterior distribution with more than one parameter dimension (mu and
# sigma each contribute one dimension) is just a multi-dimensional normal distribution. When
# you do the quadratic approximation it also calculates the covariances between each pair of
# parameters.
vcov(posterior.approx)
diag( vcov( posterior.approx ) ) # Variances aka. average of squared deciations from the mean
                          # Higher variance -> more spread -> more variance from the mean
cov2cor( vcov( posterior.approx ) ) # Correlation matrix. Lower the value the lower the correlation
                             # between the parameters is
post.samples <- extract.samples( posterior.approx , n=1e4 ) # 10,000 samples from the posterior
head(post.samples)
precis(post.samples)

# That was a Gaussian model of height and was totally boring and meaningless, now we're going
# to do something infinitesimally more interesting and look at predicting how height varies
# with the PREDICTOR VARIABLE weight.
plot(adults$height ~ adults$weight) # Can see informally how height varies with weight

# So you just take the model for height, and add a predictor variable for weight x, and then
# redefine the mean as a function of the observations of the predictor variable and the priors.
# Therefore the mean is no longer a parameter we are estimating
# And notice the regression function is the equation of a line with intercept alpha and slope
# beta (x bar is the mean/avg of the observations of x)
# "Ask the golem to find a line that relates x to h, a line that passes through alpha (intercept)
# when xi = xbar and has slope beta.

# OK define the model
xbar <- mean(adults$weight)
height.weight.model <- alist(
  height ~  dnorm(mu, sigma), # Height is normally distributed with mean mu and SD sigma
  mu     <- a + b * (weight - xbar), # The regression function that relates the priors to weight observations
  a      ~ dnorm(178, 20), # Alpha prior
  b      ~ dlnorm(0, 1), # Beta prior. Log normal  constrains beta to +ve values, -ve height makes no sense
  sigma  ~ dunif(0, 50) # Sigma prior
)
# Simulate the prior predictive distribution for the Beta prior - simulate heights from the
# model using only priors....
set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)

# Plot the lines for the pairs of alpha and beta values
plot( NULL , xlim=range(adults$weight) , ylim=c(-100,400) , xlab="weight" , ylab="height" )
abline( h=0 , lty=2 ) # Zero height
abline( h=272 , lty=1 , lwd=0.5 ) # The tallest person ever
mtext( "b ~ dnorm(0,10)" )
for ( i in 1:N ) {
  curve( a[i] + b[i]*(x - xbar),
         from=min(adults$weight), 
         to=max(adults$weight), 
         add=TRUE,
         col=col.alpha("black",0.2)
  ) 
}
# And again with the beta prior as log normally distributed:
b <- rlnorm( N , 0 , 1 )
plot( NULL , xlim=range(adults$weight) , ylim=c(-100,400) , xlab="weight" , ylab="height" )
abline( h=0 , lty=2 ) # Zero height
abline( h=272 , lty=1 , lwd=0.5 ) # The tallest person ever
mtext( "b ~ dnorm(0,10)" )
for ( i in 1:N ) {
  curve( a[i] + b[i]*(x - xbar),
         from=min(adults$weight), 
         to=max(adults$weight), 
         add=TRUE,
         col=col.alpha("black",0.2)
  ) 
}
# So the point here is that it would be very hard to see what retarded shit the bad beta prior
# is implying if you just looked at the numbers, but by doing the prior predictive simulation
# and examining the plots you can pick this up and also get an intuition of what the model's
# assumptions are.

# Run the model and examine the posterior distribution and samples from it
height.weight.posterior <- quap(height.weight.model, data=adults)
precis(height.weight.posterior) 
# The precis shows that b is 0.9 which means that a person 1kg heaver is expected to be 0.9cm
# most plausibly. Values outside 0.84 and 0.97 are highly incompatible
# Look at the covariance between parameters, very little covariance in this case:
round(vcov(height.weight.posterior), 3)

# Now let's plot the the posterior inference against the data:
plot( height ~ weight , data=adults , col=rangi2 )
post.samples <- extract.samples( height.weight.posterior , n=1e4 )
a_map <- mean(post.samples$a)
b_map <- mean(post.samples$b)
curve(a_map + b_map * (x - xbar), add=TRUE)
dens(post.samples)
