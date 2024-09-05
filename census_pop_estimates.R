# The purpose of this script is to read and transform census data si that
# the standard population of women 18+ in Puerto Rico can be calculated
# The period needed is 2005-2020

# Loading packages
library(dplyr)
library(readxl)

# Information about column content and years for renaming

years19 <- c('Census.2010', 'Base.2010', 2010:2019)
years23 <- c('Base.2020', 2020:2023)


# Loading and cleaning up format of census data files
est2009 <- read.csv("~/Downloads/pr-est00int-01.csv", skip = 2) %>%
  select(-c(April.1..20001, April.1..20102, July.1..20103)) %>% # Keep only intercensal estimates
  rename(Age_Group = Sex.and.Age) %>% # Same name as other files for merging
  slice(72:104) %>% # Keep only rows for women
mutate(Age_Group = ifelse(Age_Group == "FEMALE", "Total", Age_Group)) %>%
  filter(if_all(everything(), ~ !(is.na(.) | . == "" | . == "."))) # Removes rows with "" or NAs
  colnames(est2009)[2:11] <- as.character(2000:2009) # Gives var name from upper row
 
  
est2019 <- read_excel("~/Downloads/prc-est2019-agesex.xlsx", skip = 3) %>% 
  rename(Base.2010 = `Estimates Base`) %>% # Gives var name from upper row
  setNames(c('Age_Group', as.vector(sapply(years19, function(y) paste0(y, c(".Both", ".Male", ".Female")))))) %>% # For each period, will add the population to the colname
  slice(2:34) %>% # Removes first row that indicated population
  select(Age_Group, contains(".Female")) %>% # Keeps only women
  select(-contains("Census"), -contains("Base")) %>% # Keeps only intercensal estimates
  rename_with(~ gsub("\\.Female", "", .), contains(".Female")) %>%
  mutate(across(2:11, ~ round(as.numeric(.), 2))) %>% # Limit decimales to 2
  filter(if_all(everything(), ~ !(is.na(.) | . == "" | . == "."))) # Removes rows with "" or NAs

est2023 <- read_excel("~/Downloads/prc-est2023-agesex.xlsx", skip = 3) %>% 
  rename(Base.2020 = `...2`) %>% # Gives var name from upper row
  setNames(c('Age_Group', as.vector(sapply(years23, function(y) paste0(y, c(".Both", ".Male", ".Female")))))) %>% # For each period, will add the population to the colname
  slice(2:34) %>% # Removes first row that indicated population
  select(Age_Group, contains(".Female")) %>% # Keeps only women
  rename_with(~ gsub("\\.Female", "", .), contains(".Female")) %>%
  select(-contains("Base")) %>% # Keeps only intercensal estimates
  filter(if_all(everything(), ~ !(is.na(.) | . == "" | . == "."))) # Removes rows with "" or NAs

all_est <- Reduce(function(x, y) merge(x, y, by = "Age_Group"), list(est2009, est2019, est2023)) %>%
  distinct()

write.csv(all_est, "~/Downloads/Intercensal_female_age_grp_est_2000-2023.csv")
