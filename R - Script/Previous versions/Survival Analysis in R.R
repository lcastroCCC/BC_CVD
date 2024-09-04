

# Survival Analysis in R


# Working Directory -------------------------------------------------------

setwd("C:/Users/enavarro/Documents/BBC/Project- Dra. Carola/Data")

# Libraries ---------------------------------------------------------------

library(knitr)
library(dplyr)
library(survival)
library(ggplot2)
library(tibble)

library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

library(openxlsx)

# Data --------------------------------------------------------------------


data <- read.xlsx('C:/Users/enavarro/Documents/BBC/Project- Dra. Carola/Data/datosCarola_invasive_seq0&1_NoID.xlsx',sheet = 1)

#Age Variable is character (set to numeric)
data$AgeDx <- as.numeric(data$AgeDx)

#Set factor order for Stage at Diagnosis
data$StageAtDx <- relevel(factor(data$StageAtDx), ref = "Localized")

# CVD vs Non CVD death
data$Vital <- ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'CVD death','CVD death', 
                     ifelse(data$VitalStatus == 0 & data$BCDeathCause == 'BC death', 'BC death', 
                            ifelse(data$VitalStatus == 0, 'Other Death', 'Alive'
                            )))

# time: Delta year of diagnosis and year follow up (year of death if vital status == 0) 
# Vital Status: 1 = Alive, 0 = dead

# Recode status to 1 event (CVD death) and 0 censored
data$status <- ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'CVD death',1, 0)
data$status <- as.factor(data$status)
                            

# Calculating survival times ----------------------------------------------

data <- data %>%
  mutate(time =yy_dlc-ydiag)



# Creating survival objects and curves ------------------------------------

# The Kaplan-Meier method is the most common way to estimate survival times 
# and probabilities. It is a non-parametric approach that results in a 
# step function, where there is a step down each time an event occurs.

# The Surv() function from the {survival} package creates a survival object 
# for use as the response in a model formula. There will be one entry for each 
# subject that is the survival time, which is followed by a + if the subject was censored.

Surv(data$time, data$status)[1:20]


# The survfit() function creates survival curves using the Kaplan-Meier method 
# based on a formula.

s1 <- survfit(Surv(time, status) ~ 1, data = data)
str(s1)


# Kaplan Meier Plots ------------------------------------------------------

survfit2(Surv(time, status) ~ 1, data = data) %>% 
  ggsurvfit() +
  labs(
    x = "Years",
    y = "Overall survival probability"
  )+ 
  add_confidence_interval()+
  add_risktable()


# Estimating x -year survival ---------------------------------------------

# One quantity often of interest in a survival analysis is the probability of 
# surviving beyond a certain number of years, x

# To estimate the probability of surviving to 1
# year, use summary with the times argument
summary(survfit(Surv(time, status) ~ 1, data = data), times = 1)

# We can produce nice tables of x-time survival probability estimates 
# using the tbl_survfit() function from the {gtsummary} package:

survfit(Surv(time, status) ~ StageAtDx, data = data) %>% 
  tbl_survfit(
    times = 1,
    label_header = "**1-year survival (95% CI)**"
  )


# Comparing survival times between groups ---------------------------------

# We can conduct between-group significance tests using a log-rank test. 
# The log-rank test equally weights observations over the entire follow-up time
# and is the most common way to compare survival times between groups. 
# There are versions that more heavily weight the early or late follow-up that 
# could be more appropriate depending on the research question 


# We get the log-rank p-value using the survdiff function. For example, we 
# can test whether there was a difference in survival time according to stage at diagnosis:

survdiff(Surv(time,status)~StageAtDx, data = data)

# We see that there was a significant difference in CVD survival according to 
# stage at breast cancer diagnosis



# Cox Regression Model ----------------------------------------------------

# We may want to quantify an effect size for a single variable, or include more 
# than one variable into a regression model to account for the effects of 
# multiple variables.

# The Cox regression model is a semi-parametric model that can be used to fit 
# univariable and multivariable regression models that have survival outcomes.

# Some key assumptions of the model:
  
# non-informative censoring (not met in this data with competing events)
# proportional hazards

# We can fit regression models for survival data using the coxph()
coxph(Surv(time, status) ~ StageAtDx, data = data)

# We can obtain tables of results using the tbl_regression() function from the 
# {gtsummary} package, with the option to exponentiate set to TRUE to return 
# the hazard ratio rather than the log hazard ratio:

coxph(Surv(time, status) ~ StageAtDx, data = data) %>% 
  tbl_regression(exp = TRUE) 



# Competing Risks ---------------------------------------------------------

#There are two approaches to analysis in the presence of multiple potential outcomes:
  
# 1) Cause-specific hazard of a given event: this represents the rate 
# per unit of time of the event among those not having failed from other events

# 2) Subdistribution hazard of a given event: this represents the rate 
# per unit of time of the event as well as the influence of competing events


# To establish that a covariate is indeed acting on the event of interest, 
# cause-specific hazards may be preferred for treatment or prognostic marker effect testing. 

# To establish overall benefit, subdistribution hazards may be preferred for
# building prognostic nomograms or considering health economic effects to get a 
# better sense of the influence of treatment and other covariates on an absolute scale.


# The status variable in these data are coded in a non-standard way. Let’s recode to avoid confusion:
data <- data %>%
  mutate(
    status2 =  ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'CVD death',1, 
           ifelse(data$VitalStatus == 0, 2, 0
                  )),
    status2 = as.factor(status2)
    )


# Cumulative incidence for competing risks --------------------------------

# A non-parametric estimate of the cumulative incidence of the event of interest.
# At any point in time, the sum of the cumulative incidence of each event is equal 
# to the total cumulative incidence of any event (not true in the cause-specific setting). 
# Gray’s test is a modified Chi-squared test used to compare 2 or more groups.


# Estimate the cumulative incidence in the context of competing risks using the 
# cuminc function from the {tidycmprsk} package. By default this requires the
# status to be a factor variable with censored patients coded as 0.

cuminc(Surv(time, status2) ~ 1, data = data)

# We can use the ggcuminc() function from the {ggsurvfit} package to plot the 
# cumulative incidence. By default it plots the first event type only. 
# So the following plot shows the cumulative incidence of death from melanoma:

cuminc(Surv(time, status2) ~ 1, data = data) %>% 
  ggcuminc() +
  labs(
    x = "Years"
  ) + 
  add_confidence_interval() +
  add_risktable()



# include both event outcomes
cuminc(Surv(time, status2) ~ 1, data = data) %>% 
  ggcuminc(outcome = c("1", "2")) +
  ylim(c(0, 1)) + 
  labs(
    x = "Years"
  )




# Now let’s say we wanted to examine death from CVD or other causes in the 
# data, according to stage at breast cancer diagnosis.
# We can estimate the cumulative incidence at various times by group and display 
# that in a table using the tbl_cuminc() function from the {tidycmprsk} package, 
# and add Gray’s test to test for a difference between groups over the entire
# follow-up period using the add_p() function.


cuminc(Surv(time, status2) ~ StageAtDx, data = data) %>% 
  tbl_cuminc(
    times = 5, 
    label_header = "**{time}-year cuminc**") %>% 
  add_p()



# Plot

cuminc(Surv(time, status2) ~ StageAtDx, data = data) %>% 
  ggcuminc() + 
  labs(
    x = "Years"
  ) + 
  add_confidence_interval() +
  add_risktable()


# Competing Risks Regression ----------------------------------------------

# There are two approaches to competing risks regression:
  
# Cause-specific hazards :
# instantaneous rate of occurrence of the given type of event in subjects who are currently event‐free
# estimated using Cox regression (coxph function)

# Subdistribution hazards:
# instantaneous rate of occurrence of the given type of event in subjects who 
# have not yet experienced an event of that type 
# estimated using Fine-Gray regression (crr function)


crr(Surv(time, status2) ~ AgeDx, data = data)

crr(Surv(time, status2) ~ AgeDx, data = data) %>% 
  tbl_regression(exp = TRUE)


# Alternatively, if we wanted to use the cause-specific hazards regression approach, 
# we first need to censor all subjects who didn’t have the event of interest, 
# in this case death from CVD, and then use coxph as before. 
# So patients who died from other causes are now censored for the 
# cause-specific hazard approach to competing risks. Again we generate a table 
# of formatted results using the tbl_regression() function from the {gtsummary} package:

coxph(
  Surv(time, ifelse(status == 1, 1, 0)) ~ AgeDx, 
  data = data) %>% 
  tbl_regression(exp = TRUE)



# Assessing proportional hazards ------------------------------------------


# One assumption of the Cox proportional hazards regression model is that the 
# hazards are proportional at each point in time throughout follow-up. 
# The cox.zph() function from the {survival} package allows us to check this assumption. 

# It results in two main things:
  
# 1) A hypothesis test of whether the effect of each covariate differs according 
# to time, and a global test of all covariates at once.
# This is done by testing for an interaction effect between the covariate and log(time)
# A significant p-value indicates that the proportional hazards assumption is violated


# 2) Plots of the Schoenfeld residuals
# Deviation from a zero-slope line is evidence that the proportional hazards assumption is violated

mv_fit <- coxph(Surv(time, status == 1)~ AgeDx, data = data)
cz <- cox.zph(mv_fit)
print(cz)

plot(cz)



