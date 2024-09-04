

# Source: 
# https://cran.r-project.org/web/packages/popEpi/vignettes/sir.html


# Libraries ---------------------------------------------------------------

library(popEpi)
library(Epi)
library(dplyr)


# Data --------------------------------------------------------------------
data(sire)
data(popmort)

c <- lexpand( sire, status = status, birth = bi_date, exit = ex_date, entry = dg_date,
              breaks = list(per = 1950:2013, age = 1:100, fot = c(0,10,20,Inf)), 
              aggre = list(fot, agegroup = age, year = per, sex) )


se <- sir( coh.data = c, coh.obs = 'from0to2', coh.pyrs = 'pyrs',
           ref.data = popmort, ref.rate = 'haz', 
           adjust = c('agegroup','year','sex'), print ='fot')
se

# SMR’s for other causes is 1 for both follow-up intervals. Also the p-value
# suggest that there is no heterogeneity between SMR estimates (p=0.735).

# The total mortality can be estimated by modifying the status argument. 
# Now we want to account all deaths, i.e. status is 1 or 2.

c <- lexpand( sire, status = status %in% 1:2, birth = bi_date, exit = ex_date, entry = dg_date,
              breaks = list(per = 1950:2013, age = 1:100, fot = c(0,10,20,Inf)), 
              aggre = list(fot, agegroup = age, year = per, sex) )

se <- sir( coh.data = c, coh.obs = 'from0to1', coh.pyrs = 'pyrs',
           ref.data = popmort, ref.rate = 'haz', 
           adjust = c('agegroup','year','sex'), print ='fot')
se

# Now the estimates for follow-up intervals seems to differ significantly, 
# p = 0. Plotting SMR (S3-method for sir-object) is easily done using default plot-function.

plot(se, col = 2:3)
title('SMR for follow-up categories')

## splines
# Lets fit splines for the follow-up time and age group using two different 
# options: the splines are fitted in different model and in same model, dependent.splines.

c <- lexpand( sire, status = status %in% 1:2, birth = bi_date, exit = ex_date, entry = dg_date,
              breaks = list(per = 1950:2013, age = 1:100, fot = 0:50), 
              aggre = list(fot, agegroup = age, year = per, sex) )


sf <- sirspline( coh.data = c, coh.obs = 'from0to1', coh.pyrs = 'pyrs', 
                 ref.data = popmort, ref.rate = 'haz', 
                 adjust = c('agegroup','year','sex'),
                 spline = c('agegroup','fot'), dependent.splines=FALSE)

st <- sirspline( coh.data = c, coh.obs = 'from0to1', coh.pyrs = 'pyrs', 
                 ref.data = popmort, ref.rate = 'haz', 
                 adjust = c('agegroup','year','sex'),
                 spline = c('agegroup','fot'), dependent.splines = TRUE)

plot(sf, col=2, log=TRUE)
title('Splines fitted in different models')

plot(st, col=4, log=TRUE)
title('Splines are dependent')


# In dependent spline the fot is the ratio with zero time as reference point. 
# Reference points can be altered. Here age group profile is assumed to be same
# for every follow-up time. SMR is 0.2 times from 0 to 10 years of follow-up.

# Splines can also be stratified using the print argument. For example we split 
# the death time in two time periods and test if the age group splines are equal.

c$year.cat <- ifelse(c$year < 2002, 1, 2)
sy <- sirspline( coh.data = c, coh.obs = 'from0to1', coh.pyrs = 'pyrs', 
                 ref.data = popmort, ref.rate = 'haz', 
                 adjust = c('agegroup','year','sex'),
                 spline = c('agegroup'), print = 'year.cat')
plot(sy, log=TRUE)
legend('topright', c('before 2002','after 2002'), lty=1, col=c(1,2))


# For category before 2002 the SMR seems to be higher after the age of 50. 
# Also the p-value (<0.0001) indicates that there is a difference in age group 
# trends before and after year 2002. P-value is a likelihood ratio test that 
# compares models where splines are fitted together and separately.

print(sy)



