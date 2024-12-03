library(pwrss)

#Féminas con cáncer de seno  = 1489
#Población de féminas sin cáncer del seno = 21,149

# Calculating either sample size or power for logistic regression
pwrss.z.logreg(r2.other.x = 0.10,
               n = 200, alpha = 0.05, 
               dist = "bernoulli")

pwrss.z.logreg(p1 = 0.20, p0 = 0.14, r2.other.x = 0.10,
               n = 200, alpha = 0.05, 
               dist = "bernoulli")

# p1 = probability of being in group 1

# p0 = probability of being in group 1 without the influence of the predictors(s)
# or when the value of the predictor is zero

# r2.other.x = squared multiple correlation of 0.20 between 𝑋1 and other covariates

# dist = distribution of the predictor variable.
# Default parameters are:
#distribution = list(dist = "normal", mean = 0, sd = 1)
#distribution = list(dist = "poisson", lambda = 1)

#pwr.f2.test
# package WebPower wp.logistic
library(WebPower)
??wb.logistic

power <- wp.logistic(n = c(200, 250), p0=0.1, p1=0.15, alpha=0.05, alternative="two.sided", family="normal"); power

# pwr is for linear models though
# Install and load the pwr package
#library(pwr)

# Parameters
#n <- 200                    # Total sample size
#r2 <- 0.10                  # Proportion of variance explained by predictors
#alpha <- 0.05               # Significance level
#k <- 1                      # Number of predictors (cardiotoxic treatment)

# Calculate f2
#f2 <- r2 / (1 - r2)

# Power calculation
#power_result <- pwr.f2.test(u = k, v = n - k - 1, f2 = f2, sig.level = alpha)
#power_result
