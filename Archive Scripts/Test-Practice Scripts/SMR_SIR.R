

# Exercise 23. Calculating SMRs/SIRs


# Libraries ---------------------------------------------------------------

library(biostat3)
library(dplyr)


# SMR/SIR  ---------------------------------------------------------------

#The standardized mortality ratio (SMR) is the ratio of the observed number of deaths in the study population 
#to the number that would be expected if the study population experienced the same mortality as the standard population. 
#It is an indirectly standardized rate. When studying disease incidence the corresponding quantity is called a standardized incidence ratio (SIR).


# Practice ----------------------------------------------------------------

## A)
# Start by splitting the follow-up into 1 year age bands. We do not have a date of birth available,
#so we can make a mid-point approximation for the age at diagnosis.

data(melanoma)
head(melanoma)
mel <- filter(melanoma, stage == "Localised") %>% 
  mutate( dead = (status %in% c("Dead: cancer","Dead: other") & surv_mm <= 120)+0, 
          surv_mm = pmin(120, surv_mm)
  )


## Define the age at start and end of follow-up 
mel <- mutate( mel, adx = age+0.5,   # age at diagnosis  (mid-point approximation) 
               astart = adx, 
               astop  = adx+surv_mm/12 
)

## Split by age 
mel.split <- survSplit(mel, cut = 1:105, event = "dead", 
                       start = "astart", end = "astop")
## Quick check: the first two ids 
subset(mel.split, id<=2, select = c(id, astart, astop, dead)) 


## B)
#Now split these new records into one year calendar period bands.

# For each age time band from (a), we calculate the start and stop in calendar time 
# We calculate the time since diagnosis as difference between age at start/stop and 
# age at diagnosis, and add that interval to year at diagnosis

mel.split2 <- mutate(mel.split, 
                     ystart = ydx + astart - adx, 
                     ystop  = ydx + astop - adx
)
subset(mel.split2, id<=2, select = c(id, adx, astart,astop,dead, ydx, ystart, ystop))

## Now we can split along the calendar time 
## For each of the new age-calendar time bands, we now have to adjust the age at 
## start and end in the same way as above 
mel.split2 <- survSplit( mel.split2, cut = 1970:2000, event = "dead", 
                         start = "ystart", end = "ystop" ) %>%
  mutate( astart = adx + ystart - ydx, 
          astop  = adx + ystop - ydx
  )
## Quick check: this seems ok 
subset(mel.split2, id<=2, select = c(id, ystart, ystop, astart, astop, dead)) 

##(c)
#Descriptives: Each subject’s follow–up is now divided into small pieces 
#corresponding to the agebands and calendar periods the subject passes through. 
#We can make tables of deaths and person-years by age and calendar period with

## We calculate the total person time at risk for each time band
mel.split2 <- mutate( mel.split2, 
                      age  = floor(astart),  # Age at which person time was observed 
                      year = floor(ystart),  # Calendar year during which person time was observed 
                      pt  =  ystop - ystart  # ... or astop - astart, works the same
) 
subset(mel.split2, id<=2, select = c(id, ystart, ystop, astart, astop, dead, age, year, pt))

## Now tabulate: sum of person time across all combinations of age & year 
## (for some years, ages) 
xtabs(pt ~ age + year, data=mel.split2, subset = age>=50 & age<60 & year>=1980 & year<1990)


#As the data have been split in one-year intervals on both time scales the table 
#created above is not so informative. Grouped variables will provide a better overview.

##(d)
#Descriptives: as the data have been split in 1-year intervals on both time
#scales in the table created above, this takes up a lot of space. 
#Grouped variables will provide a better overview. To make a table of rates by 
#age and calendar period, we can define group variables and use survRate as usually:

mel.split2 <- mutate(mel.split2, 
                     age10  = cut(age, seq(0, 110 ,by=10), right=FALSE), 
                     year10 = cut(year, seq(1970, 2000, by=5), right=FALSE) 
) 
sr <- survRate(Surv(pt, dead) ~ sex + age10 + year10, data=mel.split2) 
rownames(sr) <-1:nrow(sr) ## Simple rownames for display 
head(sr, n = 20) 

##(e)
#To calculate the expected cases for a cohort, using reference mortality rates 
#classified by age and calendar period, it is first necessary to merge the 
#population rates with the observed person-time. Then the expected number of 
#cases are calculated by multiplying the follow-up time for each record by the
#reference rate for that record. The SMR is the ratio of the total observed 
#cases to the total number expected.

#First, calculate the total person time at risk and the observed number of deaths
#for each combination:


pt <- mutate(mel.split2, sex = unclass(sex)) %>%    # make sex integer to be in line with popmort 
  group_by(sex, age, year)               %>%    # aggregate by sex, age, year 
  summarise(pt = sum(pt), observed = sum(dead)) # sum the person time, deaths 

pt <- ungroup(pt)  # For convenience
head(pt) 

# We now merge the observed person time and deaths (in the melanoma cohort) 
# with the corresponding reference rates in the popmort data set:

joint <- left_join(pt, rstpm2::popmort)
head(joint)


#The expected number of events (assuming the reference mortality rates 
#in the general population) are simply:
joint <- mutate(joint, expected = pt * rate) 
head(joint) 

#(f)
#Calculate the crude overall SMR, as well as the SMR by sex. Plot the SMRs by calendar year and age.

#We can calculate the SMR simply from the data, like so:
SMR_all <- summarise(joint, SMR = sum(observed) / sum(expected) ) 
SMR_all 

# By Sex

SMR_bySex <- group_by(joint, sex) %>% summarise( SMR = sum(observed) / sum(expected) ) 
SMR_bySex 

# By Year

SMR_byYear <- group_by(joint, year) %>% summarise( SMR = sum(observed) / sum(expected) ) 
SMR_byYear 
plot( SMR_byYear, type = "o")  # quick & dirty plot 

#For the ages, we have two many levels and too few events ot produce reliable estimates.
#We can either use splines or (simpler) define a grouping variable as above.
joint <- mutate(joint, age_group = cut(age, seq(0, 110, by=10), right = FALSE))
SMR_byAge <- group_by(joint, age_group) %>% summarise( SMR = sum(observed) / sum(expected) ) 
SMR_byAge 
plot( SMR_byAge)           # quick & dirty plot 
abline( h = 1:2, lty = 2)  # two reference lines at 1 & 2

#Note that we can use survRate to get confidence intervals:
by(joint, joint$sex, function(data) poisson.test(sum(data$observed), sum(data$expected)))


#(g)
#Model the SMR as a function of age, sex and calendar period.

#We use the observed (nominator in SMR) as the outcome count and 
#the log-expected count (denominator in SMR) as offset in the Poisson regression.

#Let’s prettify the data for the fit. 
#Here, a good choice of reference level for age & year makes a big difference 
#in how easy we can interpret the model parameters;

joint2 <- transform(joint,
                    sex  = factor(sex, levels = 1:2, labels = c("m", "f")),
                    year =  factor(year) %>% relevel(ref = "1985"),  # mid-study
                    age_group = relevel(age_group, ref = "[70,80)")  # Close to one already
)
## Model & parameters
summary(fit <- glm(observed ~ sex + year + age_group + offset(log(expected)), data=joint2, family=poisson)) 
eform(fit)

#A simple analysis of deviance, which performs a likelihood ratio test 
#for the hypothesis that removing a variable from the model does not 
#improve model fit shows however that all predictors are statistically 
#significantly associated with the SMR:

drop1(fit, test = "Chisq")

