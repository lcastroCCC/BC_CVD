year <- "2015" 

# Loading dataframe
paths <- c(paste0("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Defunciones/NCHS/Procesadas/Listas/", year, ".xlsx"),
           paste0("~/Downloads/NCHS/", year, ".xlsx"),
           paste0("C:/Users/lcastro/OneDrive - Centro Comprensivo de Cancer UPR/projects_shared/BBC/Project_Dra. Carola/Data/NCHS/", year, ".xlsx"))

# Attempt to read the file
mort <- tryCatch({
  read_excel(paths[1])
}, error = function(e) {
  read_excel(paths[2])
}, error = function(e) {
  read_excel(paths[3])
})

mort2 <- mort %>% select(`Month of Death`, `Current Data Year`,
                         `Sex`, `Detail Age`, `Marital Status`, 
                         `ICD Code (10th Revision)`, `1st Condition`, `1st Condition - Code range`,
                         `2nd Condition`, `3rd Condition`) %>%
  mutate(Sex = as.factor(Sex),
         Same = ifelse(`ICD Code (10th Revision)` == `1st Condition`, TRUE, FALSE))

rd <- read_excel(paste0("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Defunciones/Registro Demografico/", year, " Defunciones al 11 Mayo 2022.xlsx"))
rd <- rd %>% select(DeathDate_Month, DeathDate_Year, 
                    Gender, Age, MaritalStatus,
                    `DeathCause_I (ID)`, `DeathCause_II (ID)`, `DeathCause_III (ID)`) %>%
  mutate(Gender = as.factor(Gender),
         MaritalStatus = case_when(
           MaritalStatus == "Divorced" ~ "D",
           MaritalStatus == "Married" ~ "M",
           MaritalStatus == "Never Married" ~ "S",
           MaritalStatus == "Unknown" ~ "U",
           MaritalStatus == "Widowed" ~ "W",
           TRUE ~ NA
         ))

t <- left_join(mort2, rd, join_by("Month of Death" == "DeathDate_Month",
                                  "Current Data Year" == "DeathDate_Year",
                                  "Sex" == "Gender",
                                  "Detail Age" == "Age",
                                  "Marital Status" == "MaritalStatus"))
t <- left_join(rd, mort2, join_by("DeathDate_Month" == "Month of Death",
                                  "DeathDate_Year" == "Current Data Year",
                                  "Gender" == "Sex",
                                  "Age" == "Detail Age",
                                  "MaritalStatus" == "Marital Status",
                                  "DeathCause_I (ID)" == "ICD Code (10th Revision)")) 
t %>% select(`DeathCause_I (ID)`, `DeathCause_II (ID)`, `DeathCause_III (ID)`, 
             `1st Condition`, `2nd Condition`, `3rd Condition`
             ) %>% View()

# Final verdict:
# ICD 10 is equal to 1st Condition
# Death Cause II is equal to 2nd Condition
# Death Cause III is equal to 3rd Condition

#table(mort2$`Current Data Year`, mort2$Same)

# 2005 T 15695 F 14007
# 2006 T
# 2007 T 29285 F 2
# 2008 T
# 2009 T
# 2010 T
# 2011 T 29919 F 1
# 2012 T
# 2013 T
# 2015 T
# 2016 T
# 2017 T
# 2018 T
# 2019 T
# 2020 T

# Testing if Annual statistics for year being evaluated matches perfectly or not

# Should be 2,790 for diabetes
# mort %>%
#   filter(grepl("^E1[0-4]", `1st Condition`)) %>% select(`ICD Code (10th Revision)`, `1st Condition`) %>%
#   nrow()
# mort %>%
#   filter(grepl("^E1[0-4]", `ICD Code (10th Revision)`)) %>% select(`ICD Code (10th Revision)`, `1st Condition`) %>%
#   nrow()
# 
# # Should be 519 for HIV
# mort %>%
#   filter(grepl("^B2[0-4]", `1st Condition`)) %>% select(`ICD Code (10th Revision)`, `1st Condition`) %>%
#   nrow()
# mort %>%
#   filter(grepl("^B2[0-4]", `ICD Code (10th Revision)`)) %>% select(`ICD Code (10th Revision)`, `1st Condition`) %>%
#   nrow()
# 
# # Should be 4,837 for malignant neoplasms
# mort %>%
#   filter(grepl("^C[00-97]", `1st Condition`)) %>% select(`ICD Code (10th Revision)`, `1st Condition`) %>%
#   nrow()
# mort %>%
#   filter(grepl("^C[00-97]", `ICD Code (10th Revision)`)) %>% select(`ICD Code (10th Revision)`, `1st Condition`) %>%
#   nrow()
# 
# Report is based on underlying cause of death:
# The disease or injury which initiated the train of events leading directly to death, or the circumstances of the accident or violence which produced the fatal injury"


