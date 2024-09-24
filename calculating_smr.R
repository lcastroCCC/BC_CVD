# Standardized mortality Ratio for CVD mortality in BC diagnosed women 18 + in Puerto Rico
# in comparison with the general female population of Puerto Rico

# Manual calculation ------------------------------------------------------


gen_1 <- suppressWarnings(tryCatch({
  read.csv("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Defunciones/cvm_rate_gen_pop1.csv")
}, error = function(e) {
  read.csv("~/Downloads/NCHS/cvm_rate_gen_pop1.csv")
}))

# Exporting general population rates by age groups 0-59, 60-69, 70-79, 80+

gen_2 <- suppressWarnings(tryCatch({
  read.csv("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Defunciones/cvm_rate_gen_pop2.csv")
}, error = function(e) {
  read.csv("~/Downloads/NCHS/cvm_rate_gen_pop2.csv")
}))

# Reading file grouped by year and age groups 0-59, 60+
cvm_by_year_agegroup1 <- suppressWarnings(tryCatch({
  read.csv("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cáncer/Data/cvm_by_year_agegroup1.csv", row.names = NULL) %>%
    mutate(Year = as.numeric(Year))
}, error = function(e) {
  read.csv("~/Downloads/Registro Central de Cáncer/cvm_by_year_agegroup1.csv", row.names = NULL) %>%
    mutate(Year = as.numeric(Year))
}))

# Reading file grouped by year and age groups 0-59, 60-69, 70-79, 80+
cvm_by_year_agegroup2 <- suppressWarnings(tryCatch({
  read.csv("C:/Users/Genesis Rodriguez/Box/Sanchez-Diaz Lab/CVD and BC/Registro Central de Cáncer/Data/cvm_by_year_agegroup2.csv", row.names = NULL) %>%
    mutate(Year = as.numeric(Year))
}, error = function(e) {
  read.csv("~/Downloads/Registro Central de Cáncer/cvm_by_year_agegroup2.csv", row.names = NULL) %>%
    mutate(Year = as.numeric(Year))
}))

left_join(gen_1, cvm_by_year_agegroup1, by = c("current_data_year" = "Year",
                                                  "AgeDeathCat" = "AgeGroup")) %>% View()


# Example dataframe with age-specific population of breast cancer survivors and general CVD mortality rates
df <- data.frame(
  age_group = c("18-44", "45-64", "65-84", "85+"),
  bc_population = c(5000, 6000, 5500, 4800), # BC Population from Cancer Registry
  bc_deaths = c(3, 7, 12, 15), # Deaths in breast cancer survivors
  gen_pop_cvd_rate = c(0.0005, 0.001, 0.002, 0.003)) %>% # General population CVD mortality rates
  mutate(expected_cvd_deaths = bc_population * gen_pop_cvd_rate) # Calculate expected CVD deaths for each age group

print(df)

# Sum the expected deaths across all age groups to get total expected CVD deaths
total_expected_deaths <- sum(df$expected_cvd_deaths)
total_observed_deaths <- sum(df$bc_deaths)
global_smr <- round(total_observed_deaths/total_expected_deaths, 2)

# Calculate variance and standard error
total_variance_SMR <- (total_observed_deaths + total_expected_deaths) / (total_expected_deaths^2)
total_SE_SMR <- sqrt(total_variance_SMR)

# Calculate 95% confidence intervals
total_CI_lower <- global_smr - 1.96 * total_SE_SMR
total_CI_upper <- global_smr + 1.96 * total_SE_SMR
total_CI <- paste0(round(total_CI_lower,2), " - ", round(total_CI_upper,2))

# Print Global SMR
tibble(Statistic = c("Observed deaths", "Expected deaths", "Global SMR"),
       Value = as.character(c(total_observed_deaths, total_expected_deaths, global_smr)),
       "95% Confidence Interval" = total_CI)





# ems package calculation ------------------------------------------------------

library(ems)

# Loading a example data
data(icu)

# Setting variable labels to data
attr(icu, "var.labels")[match(c("Unit", "IsMechanicalVentilation1h",
                                "AdmissionTypeName_pri","Vasopressors_D1"), names(icu))] <-
  c("ICU unit","Mechanichal ventilation","Admission type","Vasopressors at admission")

# Some editing
icu$Saps3DeathProbabilityStandardEquation <- icu$Saps3DeathProbabilityStandardEquation /100
icu$IsMechanicalVentilation1h <- as.factor(ifelse(icu$IsMechanicalVentilation1h == 1, "Yes", "No"))
icu$AdmissionTypeName_pri <- as.factor(icu$AdmissionTypeName_pri)
levels(icu$AdmissionTypeName_pri) <- c("Clinical","Elective surgery", "Urgent surgery")
icu$Vasopressors_D1 <- as.factor(ifelse(icu$Vasopressors_D1 == 1, "Yes", "No"))

# The overall SMR for the whole sample
SMR(icu$UnitDischargeName, icu$Saps3DeathProbabilityStandardEquation)
#### UnitDischargeName has 1 = Death and 0 = Alive
#### Saps3DeathProbabilityStandardEquation Estimates the probability of mortality for ICU patients on admission.
#### This is probably the expected probability (bc population * mortality rate for pr women/1000)

# The overall SMR and for some subgroups
x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
               pred.var = "Saps3DeathProbabilityStandardEquation",
               group.var = c( "IsMechanicalVentilation1h",
                              "AdmissionTypeName_pri","Vasopressors_D1"),
               reorder = "no",
               decreasing = TRUE,
               use.label = TRUE)
x
#### They chose as grouping variables Mechanical ventitation, type of admission and vasopressors

# A forest plot for all groups SMR (resize the window may be required)
forest.SMR(x, digits = 2)

# The same thing but reordering the categories
x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
               pred.var = "Saps3DeathProbabilityStandardEquation",
               group.var = c( "IsMechanicalVentilation1h",
                              "AdmissionTypeName_pri", "Vasopressors_D1"),
               reorder = "SMR",
               decreasing = TRUE,
               use.label = TRUE)
forest.SMR(x, digits = 2)

# The overall SMR and for all Units
x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
               pred.var = "Saps3DeathProbabilityStandardEquation",
               group.var = "Unit",
               reorder = "no",
               decreasing = TRUE,
               use.label = TRUE)
x

# A forest plot for all Units
forest.SMR(x, digits = 2)

# The same thing but reordering the categories
x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
               pred.var = "Saps3DeathProbabilityStandardEquation",
               group.var = "Unit",
               reorder = "SMR",
               decreasing = TRUE,
               use.label = TRUE)
forest.SMR(x, digits = 2)

rm(x, icu)
