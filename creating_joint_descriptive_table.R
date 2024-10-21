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
         yy_dlc = as.character(yy_dlc),
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

cancer_observed <- cancer %>%
  filter(FollowUpYears > 0) 

# NCHS General Population Data --------------------------------------------

gen_pop <- mort %>%
  filter(record_type == 1, # Resident of P.R.
         detail_age_unit == 1, # Age in years
         detail_age >= 18 &
           detail_age != 999, # 18 years +
         sex == "F") %>% # Female
  mutate(current_data_year = as.character(current_data_year),
         maritallabel = ifelse(marital_status == "M", "Married",
                               ifelse(marital_status == "U", "Unknown", "Unmarried")),
         AgeDeathCat = as.factor(case_when(detail_age >= 18 & detail_age <= 59 ~ "18-59",
                                           detail_age >= 60 & detail_age <= 69 ~ "60-69",
                                           detail_age >= 70 & detail_age <= 79 ~ "70-79",
                                           detail_age >= 80 ~ "80+",
                                           TRUE ~ NA))) %>%
  select(detail_age, current_data_year, AgeDeathCat, marital_status, maritallabel)


# Create the summary table for breast cancer patients
tbl_cancer <- cancer_observed %>%
  filter(Vital != "Alive",
         yy_dlc <= 2021) %>%
  select(AgeDeath, AgeDeathCat2, yy_dlc) %>%
  tbl_summary(
    label = c(AgeDeath ~ "Age at Death",
              yy_dlc ~ "Year of Death",
              AgeDeathCat2 = "Age group"),  # Consistent label
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    digits = list(
      all_categorical() ~ c(0, 2),
      all_continuous() ~ c(2, 2)),
    missing_text = "0"
  ) %>%
  modify_header(label = "**Breast Cancer Patients**") %>%  # Change column header
  modify_footnote(
    all_stat_cols() ~ NA  # Remove the default footnote for continuous variables
  ) %>% 
  modify_footnote(
    all_stat_cols() ~ "For continuous variables: Mean (SD). For categorical variables: n (%)."  # Add custom footnote
  )

# Create the summary table for the general population (rename 'detail_age' to 'AgeDeath' for consistency)
tbl_general <- gen_pop %>%
  rename(AgeDeath = detail_age,
         yy_dlc = current_data_year,
         AgeDeathCat2 = AgeDeathCat) %>%  # Rename to ensure consistency
  select(AgeDeath, yy_dlc, AgeDeathCat2) %>%
  tbl_summary(
    label = c(AgeDeath ~ "Age at Death",
              yy_dlc ~ "Year of Death",
              AgeDeathCat2 = "Age group"),  # Same label
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    digits = list(
      all_categorical() ~ c(0, 2),
      all_continuous() ~ c(2, 2)),
    missing_text = "0"
  ) %>%
  modify_header(label = "**General Population**")  # Change column header

# Merge the two summary tables side by side
tbl_combined <- tbl_merge(
  tbls = list(tbl_cancer, tbl_general),
  tab_spanner = c("**Breast Cancer Patients**", "**General Population**")
) %>% modify_caption("**Table 1: Baseline Characteristics of Population**") %>%
  modify_header(label = "**Characteristic**") %>%  
  modify_footnote(
    all_stat_cols() ~ NA  # Remove the default footnote for continuous variables
  ) %>% 
  modify_footnote(
    all_stat_cols() ~ "For continuous variables: Mean (SD). For categorical variables: n (%)."  # Add custom footnote
  ) %>%
  as_gt()


# Print the combined table
tbl_combined

gt::gtsave(tbl_combined, "~/Downloads/summary_table.docx")
gt::gtsave(tbl_combined, "~/Downloads/summary_table.html")
