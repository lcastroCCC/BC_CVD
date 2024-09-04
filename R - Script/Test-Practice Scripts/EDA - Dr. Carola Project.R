
# Working Directory -------------------------------------------------------

setwd("C:/Users/enavarro/Documents/BBC/Project- Dra. Carola/Data")

# Libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(survival)
library(openxlsx)

# Dataset -----------------------------------------------------------------

data <- read.xlsx('C:/Users/enavarro/Documents/BBC/Project- Dra. Carola/Data/datosCarola_invasive_seq0&1_NoID.xlsx',sheet = 1)


# EDA ---------------------------------------------------------------------


data %>%
  summarize( count = n_distinct(EncryptedID))

data %>%
  group_by(ydiag) %>%
  summarize( count = n_distinct(EncryptedID))

data %>% 
  group_by(BCDeathCause, CVDDeathCause)%>% summarize(n = n())%>% ungroup()%>% mutate( percent = (n/sum(n))*100)
data %>% 
  group_by(BCDeathCause)%>% summarize(n = n())%>% ungroup()%>% mutate( percent = (n/sum(n))*100)
data %>% 
  group_by(AgeDx)%>% summarize(n = n())%>% ungroup()%>% mutate( percent = (n/sum(n))*100)%>% View()


# Age Variable (age at diagnostic) ------------------------------------------------------------

data$AgeDx <- as.numeric(data$AgeDx) #Set as numeric

# Graph
barplot(xtabs(~AgeDx, data = data), col = 'gray90',xlab = 'Age', ylab = 'Frequency')

# Age (measures of spread)
range(data$AgeDx)
range(data[which(data$AgeDx != 999), 'AgeDx']) # Exclude unknown

quantile(data[which(data$AgeDx != 999), 'AgeDx'])
mean(data[which(data$AgeDx != 999), 'AgeDx'], na.rm = TRUE)
# Note: mean is almost = median 

sd(data[which(data$AgeDx != 999), 'AgeDx'])
mad(data[which(data$AgeDx != 999), 'AgeDx'])

#Histogram and Density 
hist(data[which(data$AgeDx != 999), 'AgeDx'], main = 'Histogram of Age', xlab = 'Age', freq =FALSE)
lines(density(data[which(data$AgeDx != 999), 'AgeDx']), col = 'red', lwd = 3)
boxplot(data[which(data$AgeDx != 999), 'AgeDx'])



# Time between Diagnostics and Vital Status and Age at Vital Status --------------------------------
data$yrBtw <- data$yy_dlc - data$ydiag #years between diagnostic and last vital status

mean(data[which(data$VitalStatus == 0), 'yrBtw'])

# Age at vital status
data$Age_dlc <- data$AgeDx + data$yrBtw
# Mean age of deceased
mean(data[which(data$VitalStatus == 0), 'Age_dlc'])
median(data[which(data$VitalStatus == 0), 'Age_dlc'])

hist(data[which(data$AgeDx != 999 & data$VitalStatus == 0), 'Age_dlc'], main = 'Histogram of Age at Death', xlab = 'Age at Death', freq =FALSE)


# Survival ----------------------------------------------------------------

Sobj <- Surv(time = data$yrBtw, event = data$VitalStatus)
## note: yr diagnostic = yr vital status in some cases

Sfit <- survfit(Sobj ~ StageAtDx, data=data,
                conf.type="log", conf.int=0.95)
print(Sfit)
summary(Sfit)

Sobj <- Surv(time = data$yrBtw, event = data$VitalStatus)

Sfit <- survfit(Sobj ~ StageAtDx, data=data,
                conf.type="log", conf.int=0.95)
print(Sfit)
summary(Sfit)

plot(Sfit, conf.int=TRUE, lty=c(1:4), col=c(1:4),
     xlab="Follow-up time",
      ylab="Survival Probability")
 legend(-8, 0.3,
          legend=c("Distant", "Localized", "Regional","Unstaged"),
          lty=c(1:4), , col=c(1:4), bty="n")



# Comparison of survival distributions
survdiff(Sobj ~ StageAtDx, data=data)

# Cox

fit <- coxph(Sobj ~ StageAtDx, data=data)
summary(fit)

resdev <- residuals(fit,type="deviance");resdev


with(data = data, ftable(BCDeathCause, CVDDeathCause))



