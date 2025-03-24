us_reference_raw <- suppressWarnings(tryCatch({
  read.delim("C:/Users/Genesis Rodriguez/Downloads/Statistical Analysis/Inputs/us_reference.txt")
}, error = function(e) {
  read.delim("~/Downloads/Statistical Analysis/Inputs/us_reference.txt")
}, error = function(e) {
  read.delim("C:/Users/lcastro/OneDrive - Centro Comprensivo de Cancer UPR/projects_shared/BBC/Project_Dra. Carola/Data/Inputs/us_reference.txt")
}))

# This comes from database:
# Mortality - All COD, Aggregated Total U.S. (1969-2022) <Katrina/Rita Population Adjustment>
# Adjusted by age, sex and year
us_reference <- us_reference_raw %>%
 # filter( Age.recode.MPS != "Unknown",
  #       Year.of.death..1975.2021. %in% 2004:2021) %>% # study period
  mutate(across(c(Crude.Rate, Year.of.death..2004.2021., Count, Population), ~ as.numeric(gsub("[^0-9.]", "", .))),
         agegroup = case_when(
           str_detect(Age.recode.MPS, "^(00|01|05|10|15|20|25|30|35)") ~ 0,
           str_detect(Age.recode.MPS, "^(40|45)") ~ 40,
           str_detect(Age.recode.MPS, "^(50|55)") ~ 50,
           str_detect(Age.recode.MPS, "^(60|65)") ~ 60,
           str_detect(Age.recode.MPS, "^(70|75)") ~ 70,
           str_detect(Age.recode.MPS, "^(80|85)") ~ 80,
           TRUE ~ NA)) %>% # our population is 18+, but age groups are 0-39, 40-49, 50-59, 60-69, 70-79, 80+
  rename(deathyear = Year.of.death..2004.2021.,
         Race = Race..WBO.) %>%
  group_by(agegroup, deathyear, Race) %>%
  summarise(Count = sum(Count, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE),
            gen_cvd_rate = sum(Count) / sum(Population), .groups = "drop")

# SEER SMR Case listing 
smr_case_listing_raw <- read.delim("~/Downloads/Statistical Analysis/Inputs/smr_case_listing.txt")
smr_case_listing <- smr_case_listing_raw %>%
  group_by(Patient.ID) %>%
  # Pull Year of Diagnosis from the Index Record or Index/Event Record
  mutate(Year.of.diagnosis = as.Date(
    paste(ifelse(any(Event.Number %in% c("Index Record", "Index/Event Record")), 
                 Year.of.diagnosis[Event.Number %in% c("Index Record", "Index/Event Record")][1], 
                 Year.of.diagnosis[1]), "01", "02", sep = "-")),
    # Pull Survival.months from Index Record or Index/Event Record if available
    Survival.months = ifelse(any(Event.Number %in% c("Index Record", "Index/Event Record")),
                             Survival.months[Event.Number %in% c("Index Record", "Index/Event Record")][1],
                             Survival.months)
  ) %>%
  # Filter rows based on conditions
  filter(
    if (any(COD.rec.with.CVD..Event.Variable. == "Alive")) {
      row_number() == n()  # Keeps last row if Alive
    } else if (any(Event.Number %in% c("Index Record", "Index/Event Record"))) {
      Event.Number %in% c("Index Record", "Index/Event Record")  # Keeps Index if not Alive
    } else {
      row_number() == 1
    }
  ) %>%
  ungroup() %>%
  # Mutate variables
  mutate(
    Survival.months = as.numeric(Survival.months),  # Ensure Survival.months is numeric
    Survival.months = ifelse(Survival.months == 11, 12, Survival.months),
    FollowUpStart = case_when(
      Year.of.death.recode == "Alive at last contact" & !is.na(Survival.months) ~ as.Date("2021-12-31") %m-% months(Survival.months - 12) - days(30),
      TRUE ~ Year.of.diagnosis + months(12)),
    FollowUpEnd = case_when(
      Year.of.death.recode == "Alive at last contact" ~ as.Date("2021-12-31"),
    #  TRUE ~ Year.of.diagnosis + months(Survival.months + 2) + days(1)), #
      TRUE ~ Year.of.diagnosis + months(Survival.months) + days(1)),
    SurvivalYears = as.numeric(difftime(FollowUpEnd, Year.of.diagnosis, units = "days")) / 365.25,
    cvdStatus = ifelse(Event.Number == "Not an Event", 0,
                       ifelse(COD.rec.with.CVD..Event.Variable. %in% c("Diseases of Heart", "Cerebrovascular Diseases", 
                                                                       "Atherosclerosis", "Aortic Aneurysm and Dissection", 
                                                                       "Other Diseases of Arteries, Arterioles, Capillaries"), 1, 0)),
    agegroup = as.numeric(substr(Age.recode.with.single.ages.and.85., 1, 2)),
    agegroup2 = case_when(
      str_detect(Age.recode.with..1.year.olds, "^(00|01|05|10|15|20|25|30|35)") ~ 0,
      str_detect(Age.recode.with..1.year.olds, "^(40|45)") ~ 40,
      str_detect(Age.recode.with..1.year.olds, "^(50|55)") ~ 50,
      str_detect(Age.recode.with..1.year.olds, "^(60|65)") ~ 60,
      str_detect(Age.recode.with..1.year.olds, "^(70|75)") ~ 70,
      str_detect(Age.recode.with..1.year.olds, "^(80|85)") ~ 80,
      TRUE ~ NA
    ),
    birth_date = as.Date(paste(Year.of.diagnosis - years(agegroup), "06", "01", sep = "-")),
    AgeFollowUpStart = agegroup + 1,
    SEER_Survival = Survival.months / 12,
    personYears = as.numeric(difftime(FollowUpEnd, FollowUpStart, units = "days")) / 365.25
  ) %>%
  filter(!(Event.Number %in% c("Prior to Index"))) %>%
  rename(Race = Race.recode..White..Black..Other.)

sum(smr_case_listing$Person.Time.Years..Calculated., na.rm = TRUE) # Should be 5,183,547
table(smr_case_listing$cvdStatus) # Should be 30,721
sum(smr_case_listing$personYears) # Getting 5,185,728

c <- lexpand(smr_case_listing, status = cvdStatus != 0,
             birth = birth_date,
             entry = FollowUpStart,
             exit = FollowUpEnd, 
             breaks = list(per = 2004:2022, age = c(0, 40, 50, 60, 70, 80, Inf)), 
             aggre = list(agegroup = age, deathyear = per, Race, Race.Ethnicity.CVD)) #Race.Ethnicity.CVD))  # 2,908 supposedly with 0 py
sum(c$pyrs) # Should be 5,183,547
sum(c$from0to1) # Should be 30,721

# SMR calculation

sir(coh.data = c, coh.obs = 'from0to1', 
    coh.pyrs = 'pyrs', 
    ref.data = us_reference, 
    ref.rate = 'gen_cvd_rate', 
    adjust = c('deathyear', 'agegroup', 'Race'), 
    test.type = "homogeneity",
    conf.type = "wald",
    conf.level = 0.95, 
    print = "Race.Ethnicity.CVD") %>% 
  mutate(seer_smrs = c(NA, 1.08, 1.03, 0.83, 0.86))

# Should be:
# 0.86 White
# 1.03 Black
# 1.08 AANHPI & AIAN 
# 0.83 US Hispanic

# Manual way of calculating SMRs
smr_table <- left_join(c, us_reference, by = c("agegroup" = "agegroup", "deathyear" = "deathyear", "Race" = "Race")) %>%
  mutate(expected = pyrs * gen_cvd_rate) %>%
  group_by(Race) %>%
  summarise(Observed = sum(from0to1, na.rm = TRUE), 
            Expected = sum(expected, na.rm = TRUE),
            pyrs = sum(pyrs, na.rm = TRUE),
            SMR = Observed/Expected,
            CI.Lower = qchisq(0.05 / 2, df = 2 * Observed) / (2 * Expected), # Exact Poisson CI formula used by SEER*Stat
            CI.Upper = qchisq(1 - 0.05 / 2, df = 2 * (Observed + 1)) / (2 * Expected)); smr_table


testing_smr_detailed <- read.delim("~/Downloads/testing_smr_detailed.txt") %>%
  mutate(Person.Years.at.Risk = as.numeric(gsub(",", "", Person.Years.at.Risk)),
         Expected = as.numeric(gsub(",", "", Expected)),
         gen_rate = Expected/Person.Years.at.Risk)
