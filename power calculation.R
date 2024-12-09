
#Féminas con cáncer de seno  = 1489
#Población de féminas sin cáncer del seno = 21,149

# p1 = probability of being in group 1

# p0 = probability of being in group 1 without the influence of the predictors(s)
# or when the value of the predictor is zero

# r2.other.x = squared multiple correlation of 0.20 between 𝑋1 and other covariates

# dist = distribution of the predictor x variable.
# Default parameters are:
#distribution = list(dist = "normal", mean = 0, sd = 1)
#distribution = list(dist = "poisson", lambda = 1)

# Inputs
nSizes <- c(200, 250, 300)
p0 <- 0.705
p1 <- 0.80
r2 <- 0.10
alpha <- 0.05


# Package pwrss -------------------------------------------------------------
library(pwrss)

power <- pwrss.z.logreg(p1 = p1, p0 = p0, r2.other.x = r2,
               n = nSizes, alpha = alpha, 
               dist = "normal", alternative = "not equal"); power
plot(power)

# Calculating a sample size instead
sample <- pwrss.z.logreg(p1 = 0.75, p0 = p0, r2.other.x = r2,
               power = 0.80, alpha = alpha, 
               dist = "normal", alternative = "not equal"); sample
plot(sample)
#pwrss.z.logreg(p0 = p0, odds.ratio = 1.18, r2.other.x = r2,sample()#pwrss.z.logreg(p0 = p0, odds.ratio = 1.18, r2.other.x = r2,
#               n = nSizes, alpha = alpha, 
#               dist = "normal")

# Package WebPower -------------------------------------------------------------
library(WebPower)

power <- wp.logistic(n = nSizes, p0 = p0, p1 = p1, alpha = alpha, alternative="two.sided", family="normal"); power
plot(power)


# Package pwr -------------------------------------------------------------
library(pwr)

# pwr is for linear models though
k <- 8                      # Number of predictors (cardiotoxic treatment)

# Calculate f2
f2 <- r2 / (1 - r2)

power_result <- pwr.f2.test(u = k, v = nSizes - k - 1, f2 = f2, sig.level = alpha)
power_result

# Aim 3 -------------------------------------------------------------------

# Estimates from:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC7218836/pdf/12911_2020_Article_1127.pdf
# These proportions are of CHD diagnosis given less than ideal CVH. With and without cardiotoxic treatment

p1 = 0.759
p0 = 0.559

pwrss.z.logreg(p1 = p1, p0 = p0, r2.other.x = r2,
                         power = 0.80, alpha = alpha, 
                         dist = "normal", alternative = "not equal")
