library(DiagrammeR)
library(glue)
library(DiagrammeRsvg)
library(rsvg)
library(magick)

# US Case Listing
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

exclude1_2 <- 4860
exclude2_2 <- 37350
include1_2 <- nrow(smr_case_listing) - smr_case_listing %>% filter(Race.Ethnicity.CVD == " ") %>% nrow()
exclude_3_2 <- smr_case_listing %>% filter(Race.Ethnicity.CVD == " ") %>% nrow()
n2 <- nrow(smr_case_listing) + exclude1_2 + exclude2_2

n2 <- comma(n2)
exclude1_2 <- comma(exclude1_2)
exclude2_2 <- comma(exclude2_2)
include1_2 <- comma(include1_2)

us_counts <- smr_case_listing %>%
  mutate(COD = case_when(
    COD.rec.with.CVD..Event.Variable. %in% c("Diseases of Heart", "Cerebrovascular Diseases", 
                                             "Atherosclerosis", "Aortic Aneurysm and Dissection", 
                                             "Other Diseases of Arteries, Arterioles, Capillaries") ~ "CVD Death",
    COD.rec.with.CVD..Event.Variable. == "Alive"~ "Alive",
    TRUE ~ "Other Causes of Death"),
    Race.Ethnicity.CVD = gsub("Asian American, Native Hawaiian and other Pacific Islanders",
                              "AANHPI & AIAN",
                              Race.Ethnicity.CVD)) %>%
  group_by(COD, Race.Ethnicity.CVD) %>%
  count(name= "Count") %>%
  rename(`Cause of Death` = COD,
         race_eth = Race.Ethnicity.CVD)

# PR Case listing
n1 <- cancer %>% nrow()
exclude1_1 <- cancer %>% filter(is.na(AgeDx)) %>% nrow()  # No age group
exclude2_1 <- cancer %>% filter(!is.na(AgeDx),
                              FollowUpYears <= 0) %>% nrow()  # Did not reach follow-up period
analyzed_1 <- n1 - exclude1_1 - exclude2_1

n1 <- comma(n1)
exclude1_1 <- comma(exclude1_1)
exclude2_1 <- comma(exclude2_1)
analyzed_1 <- comma(analyzed_1)

pr_counts <- cancer_observed %>%
  mutate(Vital = as.character(Vital),
         `Cause of Death` = case_when(
           Vital == "CVD death" ~ "CVD Death",
           Vital == "BC death" ~ "Other Causes of Death",
           Vital == "Other Death" ~ "Other Causes of Death",
           TRUE ~ Vital)) %>%
  group_by(`Cause of Death`) %>%
  count(name = "Count") %>%
  mutate(race_eth = "PR Hispanic")

# Flowchart content

white_alive <- comma(us_counts %>% filter(race_eth == "White", `Cause of Death` == "Alive") %>% pull(Count))
white_cvd <- comma(us_counts %>% filter(race_eth == "White", `Cause of Death` == "CVD Death") %>% pull(Count))
white_other <- comma(us_counts %>% filter(race_eth == "White", `Cause of Death` == "Other Causes of Death") %>% pull(Count))

black_alive <- comma(us_counts %>% filter(race_eth == "Black", `Cause of Death` == "Alive") %>% pull(Count))
black_cvd <- comma(us_counts %>% filter(race_eth == "Black", `Cause of Death` == "CVD Death") %>% pull(Count))
black_other <- comma(us_counts %>% filter(race_eth == "Black", `Cause of Death` == "Other Causes of Death") %>% pull(Count))

us_hispanic_alive <- comma(us_counts %>% filter(race_eth == "US Hispanic", `Cause of Death` == "Alive") %>% pull(Count))
us_hispanic_cvd <- comma(us_counts %>% filter(race_eth == "US Hispanic", `Cause of Death` == "CVD Death") %>% pull(Count))
us_hispanic_other <- comma(us_counts %>% filter(race_eth == "US Hispanic", `Cause of Death` == "Other Causes of Death") %>% pull(Count))

aanhpi_alive <- comma(us_counts %>% filter(race_eth == "AANHPI & AIAN", `Cause of Death` == "Alive") %>% pull(Count)) 
aanhpi_cvd <- comma(us_counts %>% filter(race_eth == "AANHPI & AIAN", `Cause of Death` == "CVD Death") %>% pull(Count)) 
aanhpi_other <- comma(us_counts %>% filter(race_eth == "AANHPI & AIAN", `Cause of Death` == "Other Causes of Death") %>% pull(Count)) 

pr_alive <- comma(pr_counts %>% filter(`Cause of Death` == "Alive") %>% pull(Count))
pr_cvd <- comma(pr_counts %>% filter(`Cause of Death` == "CVD Death") %>% pull(Count))
pr_other <- comma(pr_counts %>% filter(`Cause of Death` == "Other Causes of Death") %>% pull(Count))

svg <- grViz(
  glue("digraph my_flowchart {{ 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 2.5, height = 0.75, fontsize = 8]

      // PRCCR label as a separate node
      prccr_label[label = \"Puerto Rico Central Cancer Registry\", shape = plaintext, fontsize = 12];

      // Puerto Rico Data
      node1_1[label = <Total available women <br/>diagnosed with primary invasive BC<br/>(n = {n1})>]

      blank2_1[label = '', width = 0.01, height = 0.01]
      excluded2_1[label = <Excluded because of missing age (n = {exclude1_1})>]
      node1_1 -> blank2_1[dir = none];
      blank2_1 -> excluded2_1[minlen = 2];
      {{ rank = same; blank2_1 excluded2_1 }}

      blank3_1[label = '', width = 0.01, height = 0.01]
      excluded3_1[label = <Excluded because of not meeting minimum of<br/>12-month post-diagnosis (n = {exclude2_1})>]
      blank2_1 -> blank3_1[dir = none];
      blank3_1 -> excluded3_1
      {{ rank = same; blank3_1 excluded3_1 }}
      
      node2_1[label = <Included for analysis<br/>(n = {analyzed_1})>]
      
      node3[label = < PR Hispanic<br/>
      Alive (n = {pr_alive})<br/>
      CVD death (n = {pr_cvd})<br/>
      Other causes of death (n = {pr_other})>]
      node2_1 -> node3;
      blank3_1 -> node2_1;

      // Position PRCCR label above PR data
      prccr_label -> node1_1 [style=invis]

      // SEER
       // PRCCR label as a separate node
      seer_label[label = \"SEER Research Data, 17 Registries\", shape = plaintext, fontsize = 12];
      
      node1_2[label = <Total available women <br/>diagnosed with primary invasive BC<br/>(n = {n2})>]
      // Position SEER label above SEER data
      seer_label -> node1_2 [style=invis]
      
      blank1_2[label = '', width = 0.01, height = 0.01]
      excluded1_2[label = <Excluded because a referent population group<br/>could not be located (n = {exclude1_2})<br/>
      <br/>Excluded due to unidentified cohort table cell<br/>(n = {exclude_3_2})>]
      node1_2 -> blank1_2[dir = none];
      blank1_2 -> excluded1_2[minlen = 2];
      {{ rank = same; blank1_2 excluded1_2 }}
      
      blank2_2[label = '', width = 0.01, height = 0.01]
      excluded2_2[label = <Excluded because of not meeting minimum of<br/>12-month post-diagnosis (n = {exclude2_2})>]
      blank1_2 -> blank2_2[dir = none];
      blank2_2 -> excluded2_2[minlen = 2];
      {{ rank = same; blank2_2 excluded2_2 }}

      node2_2[label = <Included for analysis<br/>(n = {include1_2})>]
      blank2_2 -> node2_2;

     // SEER Data formatted like PR Data
      node4_1[label = < US Hispanic<br/>
      Alive (n = {us_hispanic_alive})<br/>
      CVD death (n = {us_hispanic_cvd})<br/>
      Other causes of death (n = {us_hispanic_other})>]

      node4_2[label = < White<br/>
      Alive (n = {white_alive})<br/>
      CVD death (n = {white_cvd})<br/>
      Other causes of death (n = {white_other})>]

      node4_3[label = < Black<br/>
      Alive (n = {black_alive})<br/>
      CVD death (n = {black_cvd})<br/>
      Other causes of death (n = {black_other})>]

      node4_4[label = < AANHPI and AIAN<br/>
      Alive (n = {aanhpi_alive})<br/>
      CVD death (n = {aanhpi_cvd})<br/>
      Other causes of death (n = {aanhpi_other})>]

      // Connect Dataset 2 to SEER boxes
      node2_2 -> node4_1;
      node2_2 -> node4_2;
      node2_2 -> node4_3;
      node2_2 -> node4_4;

      // Keep all SEER boxes in the same rank
      {{ rank = same; node4_1 node4_2 node4_3 node4_4 }}
      // Layout adjustment to keep everything side by side
      {{ rank = same; node1_1 node1_2 }}
      {{ rank = same; node2_1 node2_2 }}
       {{ rank = same; node3 }}
      
  }}")); svg

svg_code <- svg %>% export_svg()

raster_img <- image_read_svg(svg_code, width = 2100, height = 1100) # Adjust resolution as needed
image_write(raster_img, path = "~/Downloads/Statistical Analysis/Outputs/Graphs/Flowchart.tif", format = "tiff")

