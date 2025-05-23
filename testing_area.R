library(lubridate)
library(tidyverse)
library(gtsummary)

# Cancer Data -------------------------------------------------------------

# Paths
paths <- c("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cancer/Data/cancer_registry_20240924.csv",
           "~/Downloads/Registro Central de Cancer/cancer_registry_20240924.csv",
           paste0("C:/Users/lcastro/OneDrive - Centro Comprensivo de Cancer UPR/projects_shared/BBC/Project_Dra. Carola/Data/Registro Central de Cáncer/cancer_registry_20240924.csv"))

# Attempt to read the file
cancer.raw <- suppressWarnings(tryCatch({
  read.csv(paths[1])
}, error = function(e) {
  read.csv(paths[2])
}, error = function(e) {
  read.csv(paths[3])
}))

cancer <- cancer.raw %>% 
  mutate(AgeDx = as.numeric(cancer.raw$AgeDx),
         AgeDx = ifelse(AgeDx == 999, NA, AgeDx),
         birth_date = ydiag - AgeDx,
         AgeFollowUpStart = AgeDx + 1,   
         AgeDeath = ifelse(VitalStatus == 0,
                           AgeDx + (yy_dlc - ydiag), NA),
         mm_dx = ifelse(is.na(mm_dx), 6, mm_dx),
         mm_dlc = ifelse(is.na(mm_dlc), 6, mm_dlc),
         DiagnosisDate = make_date(year = ydiag, month = mm_dx, day = 1),
         FollowUpStart = DiagnosisDate %m+% years(1),
         FollowUpEnd = make_date(year = yy_dlc, month = mm_dlc, day = 1),
         FollowUpYears = as.numeric(difftime(FollowUpEnd, FollowUpStart, units = "days")) / 365.25,
         AgeDeathCat = as.factor(case_when(AgeDeath < 60 ~ "18-59",
                                           AgeDeath >= 60 ~ "60+",
                                           TRUE ~ NA)),
         AgeDeathCat2 = as.factor(case_when(AgeDeath >= 18 & AgeDeath <= 59 ~ "18-59",
                                            AgeDeath >= 60 & AgeDeath <= 69 ~ "60-69",
                                            AgeDeath >= 70 & AgeDeath <= 79 ~ "70-79",
                                            AgeDeath >= 80 ~ "80+",
                                            TRUE ~ NA)),
         cvdStatus = ifelse(VitalStatus == 0 & CVDDeathCause == "CVD death", 1, 0),
         Vital = as.factor(ifelse(VitalStatus == 0 & CVDDeathCause == 'CVD death','CVD death',
                                  ifelse(VitalStatus == 0 & BCDeathCause == 'BC death', 'BC death',
                                         ifelse(VitalStatus == 0, 'Other Death', 'Alive'
                                         )))),
         SurvivalTime = ifelse(VitalStatus == 1, NA,
                               ifelse(VitalStatus == 0 & FollowUpYears > 0,
                                      FollowUpYears, 0)),
         DxCity = str_to_title(DxCity),
         maritallabel = str_to_title(maritallabel)) %>%
  select(EncryptedID, 
         mm_dx, ydiag, DiagnosisDate, FollowUpStart, AgeDx, birth_date, AgeFollowUpStart, 
         mm_dlc, yy_dlc, FollowUpEnd, FollowUpYears, AgeDeath, FollowUpYears,
         AgeDeathCat, AgeDeathCat2, Vital, cvdStatus, SurvivalTime, maritallabel, Surgery, Radiotherapy, Chemotherapy,
         Comorb, DxCounty, DxCity, HealthRegion)

# Reviewing exclusions | 65
exclusions <- cancer %>%   
  filter(Vital == "CVD death",
         FollowUpYears <= 0) %>%
  select(Vital, AgeDx, DiagnosisDate, FollowUpEnd, SurvivalTime, Surgery, Radiotherapy, Chemotherapy)

# Persons observed | 17,183
cancer_observed <- cancer %>%
  filter(FollowUpYears > 0) 

# NCHS General Population Data --------------------------------------------

gen_pop <- mort %>%
  filter(record_type == 1, # Resident of P.R.
         detail_age_unit == 1, # Age in years
         detail_age >= 18 &
         detail_age != 999, # 18 years +
         sex == "F") %>% # Female
  mutate(maritallabel = ifelse(marital_status == "M", "Married",
                               ifelse(marital_status == "U", "Unknown", "Unmarried")),
         AgeDeathCat = as.factor(case_when(detail_age >= 18 & detail_age <= 59 ~ "18-59",
                                           detail_age >= 60 & detail_age <= 69 ~ "60-69",
                                           detail_age >= 70 & detail_age <= 79 ~ "70-79",
                                           detail_age >= 80 ~ "80+",
                                           TRUE ~ NA))) %>%
  select(detail_age, current_data_year, AgeDeathCat, marital_status, maritallabel)


# Marital Status by Source -------------------------------------------------


# Extract marital status of breast cancer patients observed that died
cancer_marital <- cancer_observed %>%
  filter(Vital != "Alive") %>%
  select(AgeDeath) %>%
  rename(detail_age = AgeDeath) %>%
  mutate(Source = "Breast cancer patients")

# Count occurrences by marital status in the general population data
general_pop_marital <- gen_pop %>%
  select(detail_age) %>%
  mutate(Source = "General Population")

comparison_df <- rbind(cancer_marital, general_pop_marital)

comparison_df %>%
  tbl_summary(by = Source,
              label = c(detail_age ~ "Age Group at Death"),
              statistic = list(all_categorical() ~ "{n} ({p}%)",
                               all_continuous() ~ "{mean} ({sd})"),
              digits = list(
                all_categorical() ~ c(2, 2)), # Counts as integers, percentages with 2 decimal places
              missing_text = "0")



# Other statistics --------------------------------------------------------

# Age at death summary 
## General population
summary(gen_pop$detail_age)
## Cancer observed
summary(cancer_observed$AgeDeath)

# Age at death category table
## General population
table(gen_pop$AgeDeathCat)
# Cancer observed
table(cancer_observed$AgeDeathCat2)

# Marital status table
## General population
table(gen_pop$maritallabel)
## Cancer observed
table(cancer_observed$maritallabel)


# Age group at death by marital status table
## General population
table(gen_pop$AgeDeathCat, gen_pop$maritallabel)
## Cancer observed
table(cancer_observed$AgeDeathCat2, cancer_observed$maritallabel)


# Marital status summary statistics
## General population
gen_pop %>%
  group_by(maritallabel) %>%
  summarize(
    mean_age = mean(detail_age, na.rm = TRUE),
    median_age = median(detail_age, na.rm = TRUE),
    sd_age = sd(detail_age, na.rm = TRUE),
    min_age = min(detail_age, na.rm = TRUE),
    max_age = max(detail_age, na.rm = TRUE),
    q1_age = quantile(detail_age, 0.25, na.rm = TRUE),
    q3_age = quantile(detail_age, 0.75, na.rm = TRUE),
    n = n())
## Cancer observed
cancer_observed %>%
  group_by(maritallabel) %>%
  summarize(
    mean_age = mean(AgeDeath, na.rm = TRUE),
    median_age = median(AgeDeath, na.rm = TRUE),
    sd_age = sd(AgeDeath, na.rm = TRUE),
    min_age = min(AgeDeath, na.rm = TRUE),
    max_age = max(AgeDeath, na.rm = TRUE),
    q1_age = quantile(AgeDeath, 0.25, na.rm = TRUE),
    q3_age = quantile(AgeDeath, 0.75, na.rm = TRUE),
    n = n())

