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
