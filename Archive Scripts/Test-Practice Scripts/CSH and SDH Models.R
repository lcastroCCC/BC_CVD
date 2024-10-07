

# Working Directory -------------------------------------------------------

setwd("C:/Users/enavarro/Documents/BBC/Project- Dra. Carola/Data")

# Libraries ---------------------------------------------------------------

library(survival)
library(cmprsk)
library(riskRegression)
library(prodlim)
#library(ckanr) # For IEPR Data API connection

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(openxlsx)


# Data --------------------------------------------------------------------

data <- read.xlsx('C:/Users/enavarro/Documents/BBC/Project- Dra. Carola/Data/datosCarola_invasive_seq0&1_NoID.xlsx',sheet = 1)
data %>% 
  group_by(BCDeathCause, CVDDeathCause)%>% summarize(n = n())%>% ungroup()%>% mutate( percent = (n/sum(n))*100)

#Age Variable is character (set to numeric)
data$AgeDx <- as.numeric(data$AgeDx)

# CVD vs Non CVD death
data$Vital <- ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'CVD death','CVD death', 
                     ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'BC death', 'BC death', 
                            ifelse(data$VitalStatus == 0, 'Other Death', 'Alive'
                            )))



# Descriptive Analysis ----------------------------------------------------

tab <- data %>%
  group_by(Vital)%>% 
  summarize(Age = paste0(median(AgeDx), "(", quantile(AgeDx)[2],"-", quantile(AgeDx)[4], ")"),
            `Stage at Diagnosis (Regional)` = paste0(sum(StageAtDx == 'Regional'), "(", round((sum(StageAtDx == 'Regional')/n())*100,1), ")"),
            `Stage at Diagnosis (Localized)` = paste0(sum(StageAtDx == 'Localized'), "(", round((sum(StageAtDx == 'Localized')/n())*100,1), ")"),
            `Stage at Diagnosis (Unstaged)` = paste0(sum(StageAtDx == 'Unstaged'), "(", round((sum(StageAtDx == 'Unstaged')/n())*100,1), ")"),
            `Stage at Diagnosis (Distant)` = paste0(sum(StageAtDx == 'Distant'), "(", round((sum(StageAtDx == 'Distant')/n())*100,1), ")"),
            
            `Surgery` = paste0(sum(Surgery == 'Yes'), "(", round((sum(Surgery == 'Yes')/n())*100,1), ")"),
            `Radiotherapy` = paste0(sum(Radiotherapy == 'Yes'), "(", round((sum(Radiotherapy == 'Yes')/n())*100,1), ")"),
            `Chemotherapy` = paste0(sum(Chemotherapy == 'Yes'), "(", round((sum(Chemotherapy == 'Yes')/n())*100,1), ")"),
            
            `Comorbidity (History_Myocardial infarction)`= paste0(sum(C0 == 1), "(", round((sum(C0 == 1)/n())*100,1), ")"),
            `Comorbidity (Myocardial infarction)` = paste0(sum(C1 == 1), "(", round((sum(C1 == 1)/n())*100,1), ")"), 
            `Comorbidity (Congestive heart failure)`= paste0(sum(C2 == 1), "(", round((sum(C2 == 1)/n())*100,1), ")"), 
            `Comorbidity (Peripheral vascular disease)`= paste0(sum(C3 == 1), "(", round((sum(C3 == 1)/n())*100,1), ")"),
            `Comorbidity (Cerebrovascular disease)`= paste0(sum(C4 == 1), "(", round((sum(C4 == 1)/n())*100,1), ")"),
            `Comorbidity (Chronic pulmonary disease)`= paste0(sum(C6 == 1), "(", round((sum(C6 == 1)/n())*100,1), ")"),
            `Comorbidity (Diabetes without chronic complication)`= paste0(sum(C10 == 1), "(", round((sum(C10 == 1)/n())*100,1), ")"), 
            `Comorbidity (Diabetes with chronic complication)`= paste0(sum(C11 == 1), "(", round((sum(C11 == 1)/n())*100,1), ")")

            
  ) %>%
  pivot_longer(cols = -Vital)%>%
  pivot_wider(names_from = Vital, values_from = value)%>%
  rename(Variable = name)


# Vital Statistics Data (PR Institute of Statistics) ---------------------- 

IEPR <- read.csv('https://datos.estadisticas.pr/dataset/20436273-84cc-4bf0-8e7f-d8ffeec98427/resource/c5f9292a-cc08-4066-9b53-37b7e9efbb04/download/nchs_mortality_2010.csv')

# Age Variable
IEPR$Age.Detail <- ifelse(substring(as.character(IEPR$AGE),1,1) == "1",
                          as.numeric(substring(as.character(IEPR$AGE),2:4)),0)

# Exclude unknown age deaths
IEPR <- IEPR %>% filter(age27 != 27)

# CVD ICDs
icd <- paste0("I0", 1:9, collapse = '|',paste0("I",10:78, collapse = '|'))

# Include only CVD deaths
IEPR <- IEPR %>% filter(grepl(icd, ICD10)) 

IEPR.CVD.2010 <- IEPR %>%
  mutate(AGE = ifelse(Age.Detail >= 85, 85, Age.Detail))%>%
  group_by(Sex,year,AGE)%>%
  summarize(Deaths = n())%>%
  mutate(SEX = ifelse(Sex == "F", 2, 1))

# PR Population estimates (2010)

# http://www.census.gov/programs-surveys/popest/data/data-sets.html

PRpop2010 <- fread('C:/Users/enavarro/Documents/Reference Datasets/PR Population Estimates/2010/prc-est00int-agesex (2010).csv')

PRpop2010 <- PRpop2010 %>%
  select(SEX, AGE,PROJESTIMATE2010)%>%
  filter(SEX != 0 & AGE != 999)

CVD.2010 <- IEPR.CVD.2010 %>%
  left_join(PRpop2010, by = c('SEX','AGE')) %>%
  mutate(CVD.Mortality = Deaths/PROJESTIMATE2010)

rm(IEPR.CVD.2010);rm(PRpop2010);rm(IEPR)

# SMR ---------------------------------------------------------------------

# SMR: observed number of CVD deaths among breast cancer patients divided by the 
# expected number of CVD deaths in the matched general female population in PR.


mort <- data %>% 
  filter(AgeDx != 999) %>%
  mutate(AGE = ifelse(AgeDx>=85,85,AgeDx),
         AGEgrp = cut(AGE,breaks = 
                        c(18,24,29,34,39,44,49,54,59,64,69,74,79,84,85),
                      include.lowest = TRUE)
         )%>% 
  group_by(AGEgrp) %>%
  summarize(BC.Death  = sum(BCDeathCause == 'BC death') , 
            CVD.Death = sum(CVDDeathCause == 'CVD death'),
            population = n()
  )
 

#popmort <- popEpi::popmort[year == 2013 & sex == 1,]

mort <- CVD.2010 %>%
  ungroup()%>%
  filter(SEX == 2) %>%
  filter(AGE >= 18) %>%
  mutate(AGEgrp = cut(AGE,breaks = 
                                  c(18,24,29,34,39,44,49,54,59,64,69,74,79,84,85),
                                include.lowest = TRUE)
  )%>% 
  group_by(AGEgrp)%>%
  summarize(Deaths = sum(Deaths),
            pop2010 = sum(PROJESTIMATE2010))%>%
  ungroup()%>%
  mutate(haz = Deaths/pop2010)%>%
  select(AGEgrp,haz)%>%
  right_join(mort, by = c('AGEgrp'))



# Calculate expected deaths for each group
mort <- mort %>%
  mutate(expected_deaths = (population * haz))

# total observed and expected deaths
SMR <- mort %>%
  summarize(total_observed_deaths = sum(CVD.Death),
            total_expected_deaths = sum(expected_deaths, na.rm = TRUE)
  )%>%
  mutate(SMR =  total_observed_deaths/total_expected_deaths)



# Calculate confidence intervals for SMRs
# Using a Poisson distribution to approximate the confidence intervals

alpha <- 0.05  # 95% confidence level
z <- qnorm(1 - alpha/2)
SMR <- SMR %>%
  mutate(lower_limit = total_observed_deaths - z * sqrt(total_observed_deaths),
         upper_limit =  total_observed_deaths + z * sqrt(total_observed_deaths),
         lower_CI = lower_limit / total_expected_deaths,
         upper_CI = upper_limit / total_expected_deaths,
         type = "Overall SMR"
)


# Print the results
cat("Total Observed Deaths:", SMR$total_observed_deaths, "\n")
cat("Total Expected Deaths:", SMR$total_expected_deaths, "\n")
cat("Standardized Mortality Ratio (SMR):", SMR$SMR, "\n")
cat("95% Confidence Interval for SMR: [", SMR$lower_CI, ", ", SMR$upper_CI, "]\n")

# Plot the SMRs with confidence intervals
ggplot(SMR, aes(x = SMR, y = type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Standardized Mortality Ratios",
       x = "Standardized Mortality Ratio (SMR)",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black")
  ) +
  geom_text(aes(x = SMR, label = sprintf("%.2f (%.2f - %.2f)", SMR, lower_CI, upper_CI)),
            hjust = -0.2, size = 3.5, nudge_y = 0.15, nudge_x = -0.35 ) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = unit(c(1, 4, 1, 1), "lines"))


# Analysis ----------------------------------------------------------------

# survival.time denotes the survival time: time to occurrence of 
# the first event.

# Create Variable denoting survival time:
data$survival.time <- data$yy_dlc - data$ydiag #years between diagnostic and last vital status
data$survival.time.months <- data$survival.time *12 #months between diagnostic and last vital status

# event.type is the event type indicator:
# 1: Cardiovascular death.
# 2: Non-cardiovascular death.(BC Death or other)
# 0: Censored observation: alive at end of follow-up.


# Create Event Variable:
data$event <- ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'CVD death', 1, 
                     ifelse(data$VitalStatus == 0 & data$CVDDeathCause != 'CVD death', 2, 0 ))

# Create variable denoting occurrence of cardiac death.
data$CVD.death <- ifelse(data$VitalStatus == 0 & data$CVDDeathCause == 'CVD death',1,0)

# Create variable denoting occurrence of non-cardiac death.
data$other.death <- ifelse(data$VitalStatus == 0 & data$CVDDeathCause != 'CVD death',1,0)


######################################################################
# Figure 1: Plot cumulative incidence functions for cardiovascular
# death and non-cardiovascular in the combined sample.              
######################################################################

postscript("CIF.figure.ps",horizontal=T,paper="letter")

cif1 <- cuminc(ftime=data$survival.time,fstatus=data$event,cencode=0)

plot(cif1$"1 1"$time,cif1$"1 1"$est,type="l",
     ylim=c(0,1),
     xlab="Survival time (years)",ylab="Probability",
     lty=3,col="red")
title("Figure 1. Cumulative Incidence functions")
lines(cif1$"1 2"$time,cif1$"1 2"$est,type="l",
      lty=2,col="blue")
legend("topleft",
       legend = c("Cardiovascular death (CIF)","Non-cardiovascular death 
(CIF)",
        "All-cause death (1-KM)/Sum of two CIFs"),
       lty = c(3,2,1),
       col = c("red","blue","black"),
       bty="n")

km.composite <- survfit(Surv(data$survival.time,data$event) ~ 1)
lines(km.composite$time,1-
        km.composite$surv,type="l",lty=1,col="black")



######################################################################
# Cause-Specific Hazard models.
######################################################################

# Cause-specific hazard for cardiovascular death.
csh <- coxph(Surv(time = survival.time.months,CVD.death)~AgeDx + StageAtDx + PSite +Grade + Comorb+
               Surgery + Radiotherapy + Chemotherapy + Lateral_label , 
             data = data)
summary(csh)


# Cause-specific hazard for non-cardiovascular death.
csh.2 <- coxph(Surv(time = survival.time.months,other.death)~AgeDx + StageAtDx + PSite +Grade + Comorb+
                 Surgery + Radiotherapy + Chemotherapy + Lateral_label , 
               data = data
)

summary(csh.2)



######################################################################
# Subdistribution Hazard models.
######################################################################

cov.mat <-model.matrix(~AgeDx + StageAtDx + PSite +Grade + Comorb+
                         Surgery + Radiotherapy + Chemotherapy + Lateral_label , 
                       data = data) [,-1]
# Subdistribution hazard model for cardiovascular death.
crr.1 <- crr(data$survival.time,data$CVD.death,cov.mat,failcode=1,cencode=0)

summary(crr.1)

## using riskRegression Library

shm <- FGR(Hist(time = survival.time, CVD.death)~AgeDx + StageAtDx + PSite +Grade + Comorb+
             Surgery + Radiotherapy + Chemotherapy + Lateral_label , 
           data = data)

shm

# Subdistribution hazard model for non-cardiovascular death.
crr.2 <- crr(data$survival.time,data$other.death,cov.mat,failcode=1,cencode=0)

summary(crr.2)

shm.2 <- FGR(Hist(time = survival.time, other.death)~AgeDx + StageAtDx + PSite +Grade + Comorb+
             Surgery + Radiotherapy + Chemotherapy + Lateral_label , 
           data = data)

shm.2


######################################################################
# Subdistribution Hazard model diagnostic
######################################################################










