# Libraries

library(pwr)
library(pwrss)

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
r2 <- 0.10 # standard R^2
alpha <- 0.05

# Aim 1 - Qualitative interviews -----------------------------------------------

n <- 12 # these many people will have an in depth interview prior to exploring other aims

# Aim 2 - Prevalence study -----------------------------------------------------

# How large must a sample be to estimate a proportion?
# Source: Daniel, W. W., & Cross, C. L. (2013). Biostatistics: a foundation for analysis in the health sciences. Tenth edition

se <- 2/1.96
n <- 15*(100-15)/(se^2)
N <- 1489        # Population size
p <- 0.15         # Expected prevalence
E <- 0.05        # Margin of error
z <- 1.96        # Z-score for 95% confidence
attrition <- .20 # Dropout or loss to follow-up

# Initial sample size (n0)
n0 <- (z^2 * p * (1 - p)) / E^2

# Adjusted sample size with finite population correction
n <- (N * z^2 * p * (1-p))/ (E^2 * (N - 1) + z^2 * p * (1-p)); n

# Adjust for attrition
n_adjusted <- n / (1 - attrition); n_adjusted

# Error for proportions

# Standard Error with Finite Population Correction
SEp <- sqrt((p * (1 - p)) / n_adjusted) * sqrt((N - n_adjusted) / (N - 1))

# Margin of Error
ME <- z * SEp; ME

# Output results
cat("Standard Error (with FPC):", round(SEp, 4), "\n")
cat("Margin of Error (95% CI):", round(ME, 4), "\n")

# Error for means

# Standard Error
sqrt(1/(n_adjusted-1)) * sum()

# Aim 3 - Treatments comparison in EHR data ------------------------------------

# Estimates p1 and p0 from:
# https://pmc.ncbi.nlm.nih.gov/articles/PMC7218836/pdf/12911_2020_Article_1127.pdf
# These proportions are of CHD diagnosis given less than ideal CVH. With and without cardiotoxic treatment

p1 <- 0.759
p0 <- 0.559

# For power analysis

pwrss.z.logreg(p1, p0, r2.other.x = r2,
               n = N, alpha = alpha, 
               dist = "normal", alternative = "not equal")

# For sample size calculation

pwrss.z.logreg(p1, p0, r2.other.x = r2,
               power = .80, alpha = alpha, 
               dist = "normal", alternative = "not equal")


# Using pwr package - power calculation for general linear model
f2 <- r2 / (1 - r2)
predictors <- 9  # Number of predictors (including cardiotoxic treatment and life's essential 8)

pwrtest <- pwr.f2.test(u = predictors, f2 = f2, sig.level = alpha, power = power)
n <- pwrtest$v + pwrtest$u + 1; n

power_result <- pwr.f2.test(u = predictors, v = nSizes - k - 1, f2 = f2, sig.level = alpha)
power_result

