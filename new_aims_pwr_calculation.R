
# Packages

library(pwr)

# Inputs

nSizes <- c(190, 200, 210)
r2 <- 0.10 # standard R^2
alpha <- 0.05
p1 <- 0.759 # Estimates p1 and p0 from: https://pmc.ncbi.nlm.nih.gov/articles/PMC7218836/pdf/12911_2020_Article_1127.pdf
p0 <- 0.559 # These proportions are of CHD diagnosis given less than ideal CVH. With and without cardiotoxic treatment
d <- 0.5  # assumed Cohen's d (medium effect size)
# 75.9% = non ideal cvh, cardiotoxic, has chd
# 55.9% = non ideal cvh, no cardiotoxic, has chd

f2 <- r2 / (1 - r2)
predictors <- 9  # Number of predictors (including cardiotoxic treatment and life's essential 8)

power_result <- pwr.f2.test(u = predictors, v = nSizes - predictors - 1, f2 = f2, sig.level = alpha)
power_result

# Cardiometabolic health is a score. Our y

# Power analysis for a two-proportion test for
pwrtest <- pwr.f2.test(u = predictors, f2 = f2, sig.level = alpha, power = 0.80)
n <- pwrtest$v + pwrtest$u + 1; n

pwrtest <- pwr.f2.test(u = predictors, f2 = f2, sig.level = alpha, power = 0.90)
n <- pwrtest$v + pwrtest$u + 1; n

# Power analysis for a two-proportion test if comparing categorical outcomes
power <- pwr.2p.test(h = ES.h(p1, p0), n = 100, sig.level = alpha); power

# Power analysis for a two-sample t-test to compare the means of cardiovascular 
# health between the two groups

power <- pwr.t.test(n = nSizes, d = d, sig.level = alpha, type = "two.sample", alternative = "two.sided"); power

