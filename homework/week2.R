# Statistical Rethinking Week 2 homework problems
# Isaac Stead isaac_stead@eva.mpg.de
library(rethinking)
data("Howell1")

# 1. Construct a linear regression of weight as predicted by height, using the adults (age 18
# or greater) from the Howell1 dataset. The heights listed below were recorded in the !Kung
# census, but weights were not recorded for these individuals. Provide predicted weights and
# 89% compatibility intervals for each of these individuals. That is, fill in the table
# below, using model-based predictions.
adults <- Howell1[Howell1$age >= 18,]
plot(adults$weight ~ adults$height)

xbar <- mean(adults$height)
weight.height.model <- alist(
  weight ~ dnorm(mu, sigma), 
  mu <- a + b * (height - xbar), # Regression func which relates params
  a ~ dnorm(65, 15), # Prior for mean weight/intercept, normally distr with 50 mean and SD 10
                     # semms sensible for male + female adults living a healthy nonindustrial life
  b ~ dlnorm(0, 1), # Beta prior, a.k.a what is the expected weight when height changes by 1 unit?
                    # lognormal to contrain to +ve values as ppl cant have negative weight
  sigma ~ dunif(0, 50) # Sigma prior for the stddev
)
# Simulate weights from the model using the priors only
N <- 100
a <- rnorm(N, 65, 15)
b <- rlnorm(N, 0, 1) # Still makes the simulated dist look a bit wacky...but better than norm
# Plot the lines for the pairs of alpha and beta values
plot( NULL , xlim=range(adults$height), ylim=c(-100,400), xlab="height", ylab="weight")
abline( h=0 , lty=2 ) # Zero weight
abline( h=300 , lty=1 , lwd=0.5 ) # My 600 pound life
mtext( "b ~ rlnorm(0,1)" )
for ( i in 1:N ) {
  curve( a[i] + b[i]*(x - xbar),
         from=min(adults$height), 
         to=max(adults$height), 
         add=TRUE,
         col=col.alpha("black",0.2)
  ) 
} # OK priors make sense it seems? Run model
weight.height.quap <- quap(weight.height.model, data=adults)
precis(weight.height.quap) # A person 1cm taller is expected to be 630 grams heavier

# Plot posterior against data
plot(weight ~ height, data=adults, col=rangi2)
post <- extract.samples(weight.height.quap)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map * (x - xbar), add=TRUE)

# Extract the predictions for the individuals in the problem set
height.seq <- c(140.0, 160.0, 175.0)
mu <- link(weight.height.quap, data=data.frame(height=height.seq))   
# The answer: about 36kg, 48kg and 59kg
mu.mean <- apply(mu ,2 ,mean )
mu.PI <- apply(mu, 2 ,PI ,prob=0.89 )
precis(mu.PI)
