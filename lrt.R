# Cleansed cancer data

cancer <- suppressWarnings(tryCatch({
  read.csv("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cancer/Data/cancer_cleansed.csv")
}, error = function(e) {
  read.csv("~/Downloads/Registro Central de Cancer/cancer_cleansed.csv")
}, error = function(e) {
  read.csv("C:/Users/lcastro/OneDrive - Centro Comprensivo de Cancer UPR/projects_shared/BBC/Project_Dra. Carola/Data/Registro Central de Cáncer/cancer_cleansed.csv")
}))
cancer <- cancer %>%
  mutate(FollowUpStart = as.Date(FollowUpStart),
         DiagnosisDate = as.Date(DiagnosisDate),
         FollowUpEnd = as.Date(FollowUpEnd),
         maritallabel = str_to_title(ifelse(maritallabel == "unknown",
                                            NA,
                                            maritallabel)),
         Comorb.cat = factor(ifelse(Comorb == '0' , 'No', 
                                    ifelse(as.numeric(Comorb) >= 1, 'Yes', 'Unknown')),
                             levels= c('Yes','No','Unknown')),
         Procedure = factor(Procedure,
                            levels = c("Surgery Only", "Chemotherapy Only",
                                       "Radiotherapy Only", "Chemotherapy and Radiotherapy",
                                       "Other")),
         Comorbidity = case_when(
           Comorb == -1 ~ NA_character_,
           Comorb == 0 ~ "0",
           Comorb == 1 ~ "1",
           Comorb == 2 ~ "2",
           Comorb >= 3 ~ "3+"
         ),
         StageAtDx = factor(StageAtDx,
                            levels = c("Localized", "Regional", "Distant", "Unstaged"),
                            labels = c("Localized", "Regional", "Distant", "Unknown/unstaged")))

cancer_observed <- cancer %>%
  filter(FollowUpYears > 0)

surv_sets <- list(
  "<4 years"   = cancer_observed %>% filter(lagTime < 4),
  "4-<6 years" = cancer_observed %>% filter(lagTime >= 4, lagTime < 6),
  "6-<8 years" = cancer_observed %>% filter(lagTime >= 6, lagTime < 8),
  "8-<10 years"= cancer_observed %>% filter(lagTime >= 8, lagTime < 10),
  "10+ years"  = cancer_observed %>% filter(lagTime >= 10)
)

data <- cancer_observed %>% filter(lagTime < 4)



model1 <- coxph(Surv(FollowUpYears, status) ~ StageAtDx + Procedure + maritallabel, 
               data = data %>% filter(AgeDxCatRecode == "<60")); model1
broom::tidy(model1, conf.int = TRUE, exponentiate = TRUE)

model2 <- coxph(Surv(FollowUpYears, status) ~ StageAtDx + Procedure + maritallabel, 
               data = data %>% filter(AgeDxCatRecode == "60+")); model2
broom::tidy(model2, conf.int = TRUE, exponentiate = TRUE)

lrt_result <- anova(model1, model2, test = "LRT")
print(lrt_result)

# en base completa, variable con intervalo de lag time y verificar interaccion con edad, intervalo y etapa. te envio eso.
# cumulative mortality by age groups, la misma de email
cancer_observed <- cancer_observed %>%
  mutate(lagTimeCat = case_when(
      lagTime < 4              ~ "1-3 years",
      lagTime >= 4 & lagTime < 6  ~ "4-5 years",
      lagTime >= 6 & lagTime < 8  ~ "6-7 years",
      lagTime >= 8 & lagTime < 10 ~ "8-9 years",
      lagTime >= 10               ~ "10+ years",
      TRUE                        ~ NA_character_),
      AgeDxCatRecode = case_when(
          AgeDx < 60 ~ "<70",
          AgeDx >= 60 ~ "70+"
        ),
      lagTimeCat2 = case_when(
        lagTime < 3 ~ "1-2",
        lagTime >= 3 ~ "3+",
        TRUE ~ NA_character_))


# Interaccion de lag time con edad

# Modelo de Cox
# Modelo reducido
reduced_model_coxph <- coxph(Surv(FollowUpYears, status) ~ lagTimeCat2 + AgeDxCatRecode, data = cancer_observed); reduced_model_coxph

# Modelo con interaccion
full_model_coxph <- coxph(Surv(FollowUpYears, status) ~ lagTimeCat2 * AgeDxCatRecode, data = cancer_observed); full_model_coxph

# Likelihood Ratio Test (LRT)
anova(reduced_model_coxph, full_model_coxph, test = "LRT")

# Modelo de Cox - Firth's penalized maximum likelihood bias reduction method
# Modelo reducido
reduced_model_firth <- coxphf(Surv(FollowUpYears, status) ~ lagTimeCat2 + AgeDxCatRecode, data = cancer_observed); reduced_model_firth

# Modelo con interaccion
full_model_firth <- coxphf(Surv(FollowUpYears, status) ~ lagTimeCat2 * AgeDxCatRecode, data = cancer_observed,
                           maxstep = 0.5, maxit = 500); full_model_firth

# Likelihood Ratio Test (LRT)
anova(reduced_model_firth, full_model_firth, test = "LRT")


# Interaccion de lag time con etapa

# Modelo de Cox
# Modelo reducido
reduced_model_coxph <- coxph(Surv(FollowUpYears, status) ~ lagTimeCat2 + StageAtDx, data = droplevels(subset(cancer_observed, StageAtDx != "Unknown/unstaged"))); reduced_model_coxph

# Modelo con interaccion
full_model_coxph <- coxph(Surv(FollowUpYears, status) ~ lagTimeCat2 * StageAtDx, data = droplevels(subset(cancer_observed, StageAtDx != "Unknown/unstaged"))); full_model_coxph

# Likelihood Ratio Test (LRT)
anova(reduced_model_coxph, full_model_coxph, test = "LRT")

# Modelo de Cox - Firth's penalized maximum likelihood bias reduction method
# Modelo reducido
reduced_model_firth <- coxphf(Surv(FollowUpYears, status) ~ lagTimeCat2 + StageAtDx, data = droplevels(subset(cancer_observed, StageAtDx != "Unknown/unstaged"))); reduced_model_firth

# Modelo con interaccion
full_model_firth <- coxphf(Surv(FollowUpYears, status) ~ lagTimeCat2 * StageAtDx, data = droplevels(subset(cancer_observed, StageAtDx != "Unknown/unstaged")), 
                           maxstep = 0.5, maxit = 500); full_model_firth

# Likelihood Ratio Test (LRT)
anova(reduced_model_firth, full_model_firth, test = "LRT")


# Pendiente: cumulative mortality by age groups, la misma de email