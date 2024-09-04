

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

library(tidyr)
library(openxlsx)
library(knitr)

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

tab %>%
  kable()



