
library(haven)
library(dplyr)
library(sandwich)
library(lmtest)
library(broom)

data <- read_dta("~/Downloads/BMI QoL WCHS/baseline_wtchangestrat_updt_2025 (1).dta")

# Create quartiles
data <- data %>%
  mutate(
    waist_qt = ntile(waist_fnl, 4),
    waisthipratio_qt = ntile(waisthip_ratio_fnl, 4),
    fatper_qt = ntile(ta_fatperc_fnl, 4),
    fatmassindex_qt = ntile(ta_fatmassindex, 4)
  )

qol_vars <- c("totals", "pwb", "fwb", "sfwb", "ewb", "bcs")

for (var in qol_vars) {
  var_x2 <- paste0(var, "_x2")
  
  # Frequency table for binary outcome
  print(table(data[[var_x2]], useNA = "always"))
  
  # Cross-tab of original vs. dichotomized
  print(table(data[[var]], data[[var_x2]], useNA = "always"))
}


vars <- c("bmi_cat6", "fatmassindex_qt", "waisthipratio_qt", "fatper_qt", "waist_qt")

for (var in vars) {
  # Construct formula
  f <- as.formula(
    paste0("totals_x2 ~ factor(", var, ") + age_fnl + factor(educ) + factor(income_cat) + menopause_fnl + factor(marital_cat3) + eversmok_final + factor(stage_rec4) + surgery_fnl + chemo_fnl + factor(subtype_fnl) + diabetes_fnl")
  )
  
  # Fit Poisson model
  model <- glm(f, data = data, family = poisson())
  
  # Robust SE
  coeftest(model, vcov = vcovHC(model, type = "HC0"))
}

irrs <- exp(coef(model))

# Create a data frame
irr_table <- data.frame(
  Variable = names(irrs),
  IRR = round(irrs, 2),
  row.names = NULL); irr_table


#v2

# Load data
data <- read_dta("~/Downloads/BMI QoL WCHS/baseline_wtchangestrat_updt_2025 (1).dta") %>%
  mutate(
    waist_qt = ntile(waist_fnl, 4),
    waisthipratio_qt = ntile(waisthip_ratio_fnl, 4),
    fatper_qt = ntile(ta_fatperc_fnl, 4),
    fatmassindex_qt = ntile(ta_fatmassindex, 4)
  ) %>%
  filter(!is.na(totals_x2))

# Variables of interest
vars <- c("bmi_cat6",  "waist_qt",  "waisthipratio_qt", "fatper_qt", "fatmassindex_qt")

# Collect results
all_results <- list()

for (var in vars) {
  # Make sure var is a factor (for consistent reference)
  data[[var]] <- factor(data[[var]])
  ref_level <- levels(data[[var]])[1]
  
  # Model formula
  f <- as.formula(
    paste0("totals_x2 ~ ", var, " + age_fnl + factor(educ) + factor(income_cat) + menopause_fnl + factor(marital_cat3) + eversmok_final + factor(stage_rec4) + surgery_fnl + chemo_fnl + factor(subtype_fnl) + diabetes_fnl")
  )
  
  # Fit model
  model <- glm(f, data = data, family = poisson())
  
  # Tidy with robust SEs
  tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(grepl(paste0("^", var), term)) %>%
    mutate(Level = gsub(paste0(var), "", term),
           Level = gsub("^", "", Level)) %>%
    mutate(Exposure = var,
           Count = sapply(Level, function(lvl) sum(data[[var]] == lvl, na.rm = TRUE))) %>%
    select(Exposure, Level, IRR = estimate, CI_low = conf.low, CI_high = conf.high, Count)
  
  # Add reference row
  ref_row <- data.frame(
    Exposure = var,
    Level = ref_level,
    IRR = 1,
    CI_low = NA,
    CI_high = NA,
    Count = sum(data[[var]] == ref_level, na.rm = TRUE)
  )
  
  # Combine and sort
  result <- bind_rows(ref_row, tidy_model)
  all_results[[var]] <- result
}

# Final table
final_results <- bind_rows(all_results)

# Print
print(final_results)
nrow(data)

table(data$depression_cat_fu1)
table(data$anxiety_cat_fu1)
