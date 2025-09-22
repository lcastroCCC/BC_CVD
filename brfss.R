
# Libraries ####

library(haven)
library(survey)
library(dplyr)
library(readxl)
library(corrplot)
library(zoo)
library(tidyr)

# Inputs ####

men_aapc <- read_excel("~/Downloads/obesity related cancers/Obesity-associated cancer incidence_up to 2022.xlsx", sheet = 2, col_names = FALSE)
colnames(men_aapc) <- c("Joinpoint", "Year", "Observed_Rate", "Sim_Rate")
young_women_aapc <- read_excel("~/Downloads/obesity related cancers/Obesity-associated cancer incidence_up to 2022.xlsx", sheet = 5, col_names = FALSE)
colnames(young_women_aapc) <- c("Joinpoint", "Year", "Observed_Rate", "Sim_Rate")

years <- 2002:2023
paths <- paste0("~/Downloads/BRFSS Data/LLCP", years, ".XPT")

for (i in seq_along(years)) {
  df_name <- paste0("brfss_", years[i])
  if (!exists(df_name, envir = .GlobalEnv)) {
    df <- read_xpt(paths[i]) %>% filter(`_STATE` == 72)
    assign(df_name, df, envir = .GlobalEnv)
  }
}

# BMI proportions in Puerto Rico by year ####

get_bmi_pct_pr <- function(years) {
  options(survey.lonely.psu = "adjust")
  
  # Explicit BMI var for years < 2004
  bmi_var_lookup <- c(
    '2002' = "_BMI2",
    '2003' = "_BMI3"
  )
  
  bind_rows(lapply(years, function(yr) {
    df_name <- paste0("brfss_", yr)
    if (!exists(df_name)) return(NULL)
    
    df <- get(df_name)
    
    if (yr <= 2010) {
      # For years before 2004, use the lookup; for 2004 and after (but ≤2010), use "_BMI4"
      bmi_var <- if (yr < 2004) {
        bmi_var_lookup[as.character(yr)]
      } else {
        "_BMI4"
      }
      
      if (is.na(bmi_var)) bmi_var <- "_BMI4"  # fallback just in case
      
      if (!all(c("_STSTR", "_FINALWT", bmi_var) %in% names(df))) return(NULL)
      
      df <- df %>%
        rename(`_LLCPWT` = `_FINALWT`) %>%
        mutate(`_BMI5CAT` = case_when(
          .data[[bmi_var]] < 1850 ~ 1,
          .data[[bmi_var]] < 2500 ~ 2,
          .data[[bmi_var]] < 3000 ~ 3,
          .data[[bmi_var]] <= 9998 ~ 4,
          TRUE ~ NA_real_
        ))
    }
    
    req_vars <- c("_STSTR", "_LLCPWT", "_BMI5CAT")
    if (any(!req_vars %in% names(df))) return(NULL)
    
    df$BMI5CAT <- factor(df$`_BMI5CAT`, levels = 1:4,
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))
    
    dsgn_pr <- svydesign(id = ~1, strata = ~`_STSTR`, weights = ~`_LLCPWT`, data = df)
    
    bmi_pct <- svymean(~BMI5CAT, dsgn_pr, na.rm = TRUE)
    ci <- confint(bmi_pct)
    pct <- round(100 * coef(bmi_pct), 1)
    ci_low <- round(100 * ci[, 1], 1)
    ci_up  <- round(100 * ci[, 2], 1)
    
    data.frame(
      year = yr,
      Category = gsub("BMI5CAT", "", names(pct)),
      Percentage = pct,
      CI = paste0("(", ci_low, "–", ci_up, ")"),
      row.names = NULL
    )
  }))
}

bmi_pr <- get_bmi_pct_pr(years)

ggplot(bmi_pr, aes(x = year, y = Percentage, color = Category)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  labs(
    title = "Trends in BMI Categories in Puerto Rico",
    x = "Year",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Underweight" = "#FECC5C",  # yellow-orange
      "Normal"      = "#FD8D3C",  # orange
      "Overweight"  = "#E31A1C",   # red
      "Obese"       = "#B10026"   # dark red
    )
  )
# BMI proportions in Puerto Rico by year and sex ####

get_bmi_pct_pr_by_sex <- function(years) {
  options(survey.lonely.psu = "adjust")
  
  bmi_var_lookup <- c(
    '2002' = "_BMI2",
    '2003' = "_BMI3"
  )
  
  bind_rows(lapply(years, function(yr) {
    df_name <- paste0("brfss_", yr)
    if (!exists(df_name)) {
      warning(paste("Data", df_name, "not found; skipping year", yr))
      return(NULL)
    }
    df <- get(df_name)
    
    sex_var <- if (yr < 2018) "SEX" else if (yr == 2018) "SEX1" else "SEXVAR"
    wt_var  <- if (yr <= 2010) "_FINALWT" else "_LLCPWT"
    
    if (!"_BMI5CAT" %in% names(df)) {
      # Select BMI variable based on year
      bmi_var <- if (yr <= 2010) {
        if (yr < 2004) {
          bmi_var_lookup[as.character(yr)]
        } else {
          "_BMI4"
        }
      } else {
        # For years after 2010, fallback logic (could customize if you want)
        if ("_BMI4" %in% names(df)) "_BMI4" else {
          warning(paste("BMI variable not found for year", yr, "; skipping"))
          return(NULL)
        }
      }
      
      if (is.na(bmi_var)) {
        warning(paste("BMI variable missing for year", yr, "; skipping"))
        return(NULL)
      }
      
      if (!bmi_var %in% names(df)) {
        warning(paste("BMI variable", bmi_var, "not found in", df_name, "; skipping"))
        return(NULL)
      }
      
      df <- df %>%
        mutate(`_BMI5CAT` = case_when(
          .data[[bmi_var]] < 1850 ~ 1,
          .data[[bmi_var]] < 2500 ~ 2,
          .data[[bmi_var]] < 3000 ~ 3,
          .data[[bmi_var]] <= 9998 ~ 4,
          TRUE ~ NA_real_
        ))
    }
    
    req_vars <- c("_STSTR", wt_var, "_BMI5CAT", sex_var)
    if (any(!req_vars %in% names(df))) {
      warning(paste("Missing required variables in", df_name, "; skipping year", yr))
      return(NULL)
    }
    
    df$BMI5CAT <- factor(df[["_BMI5CAT"]], levels = 1:4,
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))
    df$SEX <- factor(df[[sex_var]], levels = 1:2, labels = c("Male", "Female"))
    
    dsgn <- svydesign(id = ~1, strata = ~`_STSTR`, weights = df[[wt_var]], data = df)
    
    bind_rows(lapply(levels(df$SEX), function(sex) {
      dsgn_sex <- subset(dsgn, SEX == sex)
      bmi_pct <- svymean(~BMI5CAT, dsgn_sex, na.rm = TRUE)
      ci <- confint(bmi_pct)
      pct <- round(100 * coef(bmi_pct), 1)
      ci_low <- round(100 * ci[, 1], 1)
      ci_up  <- round(100 * ci[, 2], 1)
      
      data.frame(
        year = yr,
        Sex = sex,
        Category = gsub("BMI5CAT", "", names(pct)),
        Percentage = pct,
        CI = paste0("(", ci_low, "–", ci_up, ")"),
        row.names = NULL
      )
    }))
  }))
}

bmi_pr_sex <- get_bmi_pct_pr_by_sex(years)

bmi_pr_sex %>% pivot_wider(names_from = Sex,
                           values_from = c(Percentage, CI)) %>%
  relocate(CI_Male, .after = Percentage_Male) %>%
  View()

# BMI proportions in Puerto Rico by year, age and sex ####

get_bmi_by_sex_age <- function(years) {
  options(survey.lonely.psu = "adjust")
  
  bmi_var_lookup <- c(
    '2002' = "_BMI2",
    '2003' = "_BMI3"
  )
  
  bind_rows(lapply(years, function(yr) {
    df_name <- paste0("brfss_", yr)
    if (!exists(df_name)) {
      warning(paste("Data", df_name, "not found; skipping year", yr))
      return(NULL)
    }
    df <- get(df_name)
    
    sex_var <- if (yr < 2018) "SEX" else if (yr == 2018) "SEX1" else "SEXVAR"
    wt_var  <- if (yr <= 2010) "_FINALWT" else "_LLCPWT"
    
    # Create _BMI5CAT if not available
    if (!"_BMI5CAT" %in% names(df)) {
      bmi_var <- if (yr <= 2010) {
        if (yr < 2004) {
          bmi_var_lookup[as.character(yr)]
        } else {
          "_BMI4"
        }
      } else {
        if ("_BMI4" %in% names(df)) "_BMI4" else {
          warning(paste("BMI variable not found for year", yr, "; skipping"))
          return(NULL)
        }
      }
      
      if (is.na(bmi_var) || !bmi_var %in% names(df)) {
        warning(paste("BMI variable", bmi_var, "not found in", df_name, "; skipping"))
        return(NULL)
      }
      
      df <- df %>%
        mutate(`_BMI5CAT` = case_when(
          .data[[bmi_var]] < 1850 ~ 1,
          .data[[bmi_var]] < 2500 ~ 2,
          .data[[bmi_var]] < 3000 ~ 3,
          .data[[bmi_var]] <= 9998 ~ 4,
          TRUE ~ NA_real_
        ))
    }
    
    # Define age group
    if (yr < 2013) {
      if (!"AGE" %in% names(df)) {
        warning(paste("AGE variable missing in", df_name, "; skipping"))
        return(NULL)
      }
      df$AGEGROUP <- cut(df$AGE, breaks = c(-Inf, 49, Inf), labels = c("20–49", "50+"), right = TRUE)
    } else {
      if (!"_AGEG5YR" %in% names(df)) {
        warning(paste("_AGEG5YR variable missing in", df_name, "; skipping"))
        return(NULL)
      }
      df$AGEGROUP <- case_when(
        df$`_AGEG5YR` %in% 1:6 ~ "20–49",
        df$`_AGEG5YR` %in% 7:13 ~ "50+",
        TRUE ~ NA_character_
      )
    }
    
    # Check required variables
    req_vars <- c("_STSTR", wt_var, "_BMI5CAT", sex_var, "AGEGROUP")
    if (any(!req_vars %in% names(df))) {
      warning(paste("Missing required variables in", df_name, "; skipping year", yr))
      return(NULL)
    }
    
    df$BMI5CAT <- factor(df[["_BMI5CAT"]], levels = 1:4,
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))
    df$SEX <- factor(df[[sex_var]], levels = 1:2, labels = c("Male", "Female"))
    
    dsgn <- svydesign(id = ~1, strata = ~`_STSTR`, weights = df[[wt_var]], data = df)
    
    bind_rows(lapply(levels(df$SEX), function(sex) {
      lapply(c("20–49", "50+"), function(age_group) {
        dsgn_sub <- subset(dsgn, SEX == sex & AGEGROUP == age_group)
        bmi_pct <- svymean(~BMI5CAT, dsgn_sub, na.rm = TRUE)
        ci <- confint(bmi_pct)
        pct <- round(100 * coef(bmi_pct), 1)
        ci_low <- round(100 * ci[, 1], 1)
        ci_up  <- round(100 * ci[, 2], 1)
        
        data.frame(
          year = yr,
          Sex = sex,
          AgeGroup = age_group,
          Category = gsub("BMI5CAT", "", names(pct)),
          Percentage = pct,
          CI = paste0("(", ci_low, "–", ci_up, ")"),
          row.names = NULL
        )
      }) %>% bind_rows()
    })) %>% bind_rows()
  }))
}

bmi_pr_sex_age <- get_bmi_by_sex_age(years)

# Correlation ####

## Obesity Men ####
obesity_men <- bmi_pr_sex %>%
  filter(Sex == "Male", Category == "Obese") %>%
  select(year, Obesity_Pct = Percentage)

cancer_men <- men_aapc %>%
  filter(grepl("Obesity-associated", Joinpoint)) %>%
  select(year = Year, Observed_Rate) %>%
  mutate(obesity_year = year - 15)

# 3. Join cancer data (year t) to obesity data (year t−5)
joined_df <- inner_join(cancer_men, obesity_men, by = c("obesity_year" = "year"))


# 4. Correlation
cor(joined_df$Observed_Rate, joined_df$Obesity_Pct, use = "complete.obs")

ggplot(joined_df, aes(x = Obesity_Pct, y = Observed_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Correlation between Obesity Prevalence and ORC Incidence (Men)",
    x = "Obesity Prevalence (%)",
    y = "Observed ORC Incidence Rate"
  ) +
  theme_minimal()

## Obesity Young Women ####
obesity_y_women <- bmi_pr_sex_age %>%
  filter(Sex == "Female", AgeGroup == "20–49", Category == "Obese") %>%
  select(year, Obesity_Pct = Percentage)

cancer_y_women <- young_women_aapc %>%
  filter(grepl("Obesity-associated", Joinpoint)) %>%
  select(year = Year, Observed_Rate) %>%
  mutate(obesity_year = year - 15)

# 3. Join cancer data (year t) to obesity data (year t−5)
joined_df <- inner_join(cancer_y_women, obesity_y_women, by = c("obesity_year" = "year"))

# 4. Correlation
cor(joined_df$Observed_Rate, joined_df$Obesity_Pct, use = "complete.obs")

ggplot(joined_df, aes(x = Obesity_Pct, y = Observed_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Correlation between Obesity (BMI >= 30) Prevalence and ORC Incidence \nin Young Women in Puerto Rico",
    x = "Obesity Prevalence (%)",
    y = "Observed ORC Incidence Rate"
  ) +
  theme_minimal()

# Castaneda-Avila variables ### ----------------------------------------------
demo_vars <- c("_STATE", "SEXVAR", "SEX1", "_IMPRACE", "WEIGHT2", "HEIGHT3", "GENHLTH", "PHYSHLTH", "MENTHLTH", "POORHLTH",
  "_BMI5CAT", "_RFBMI5") # Demographic and QoL
cancer_screening_vars <- c("LCSCTSC1", "LCSSCNCR", "LCSCTWHN", "PSATEST1", "PSATIME1", "PCPSARS2",
                           "PSASUGS1", # Lung cancer screening
                           "HADMAM", "HOWLONG", "CERVSCRN", "CRVCLCNC",
                           "CRVCLPAP", "CRVCLHPV", "HADHYST2", #Breast and cervical cancer screening
                           "HADSIGM4", "COLNSIGM", "COLNTES1", "SIGMTES1", 
                           "LASTSIG4", "COLNCNCR", "VIRCOLO1", "VCLNTES2", 
                           "SMALSTOL", "STOLTEST", "STOOLDN2", # Colorectal cancer screening
                           "CNCRDIFF", "CNCRAGE", "CNCRTYP2", "CSRVTRT3",
                           "CSRVDOC1", "CSRVSUM", "CSRVRTRN", "CSRVINST",
                           "CSRVINSR", "CSRVDEIN", "CSRVCLIN", "CSRVPAIN",
                           "CSRVCTL2", # Cancer survivorship
                           "_STSTR", "_LLCPWT", "_PSU") # For stratification, weighting, primary sampling unit



# Correlation matrix ####


library(ggplot2)
library(dplyr)
library(stringr)

# 1. Prepare obesity + overweight data for men
obesity_men <- bmi_pr_sex %>%
  filter(Sex == "Male", Category %in% c("Obese", "Overweight")) %>%
  group_by(year) %>%
  summarise(Obesity_Pct = sum(Percentage, na.rm = TRUE)) %>%
  ungroup()

# 2. Prepare cancer data
cancer_men <- men_aapc %>%
  filter(grepl("Obesity-associated", Joinpoint)) %>%
  select(year = Year, Overall = Observed_Rate)

# 3. Function to compute Spearman correlation
get_lag_corr <- function(lag_years) {
  df <- cancer_men %>%
    mutate(obesity_year = year - lag_years) %>%
    inner_join(obesity_men, by = c("obesity_year" = "year"))
  
  if (nrow(df) == 0) stop("No matching data after join")
  
  test <- cor.test(df$Overall, df$Obesity_Pct, method = "spearman", exact = FALSE)
  cor_val <- test$estimate
  p_val <- test$p.value
  
  stars <- if (p_val < 0.001) {
    "***"
  } else if (p_val < 0.01) {
    "**"
  } else if (p_val < 0.05) {
    "*"
  } else {
    ""
  }
  
  data.frame(
    Lag = paste0("Overweight and obesity - ", lag_years, "-year lag"),
    Correlation = cor_val,
    P_value = p_val,
    Stars = stars,
    stringsAsFactors = FALSE
  )
}

# 4. Compile correlation results
cor_data <- bind_rows(
  get_lag_corr(5),
  get_lag_corr(10),
  get_lag_corr(15)
) %>%
  mutate(
    Lag = factor(Lag, levels = c("Overweight and obesity - 5-year lag", 
                                 "Overweight and obesity - 10-year lag", 
                                 "Overweight and obesity - 15-year lag")),
    Label = paste0(round(Correlation, 2), Stars)
  )

# 5. Plot
ggplot(cor_data, aes(x = Lag, y = "Overall", fill = Correlation)) +
  geom_tile(color = "white", width = 0.8, height = 0.2) +
  geom_text(aes(label = Label),
            fontface = ifelse(cor_data$Stars != "", "bold", "plain"),
            size = 6) +
  scale_fill_gradient2(low = "cornflowerblue", high = "firebrick", mid = "white", midpoint = 0,
                       limits = c(-1, 1), name = "Spearman\nCorrelation") +
  theme_minimal() +
  labs(
    x = "", y = "ORC Incidence Rate",
    caption = "* p < 0.05   ** p < 0.01   *** p < 0.001"
  ) +
  coord_fixed(ratio = 0.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(size = 14),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10, margin = margin(t = 10)))


# Yearly correlations

# Function to compute rolling correlations for a given lag
get_rolling_corr <- function(lag_years, window_size = 5) {
  df <- cancer_men %>%
    mutate(obesity_year = year - lag_years) %>%
    inner_join(obesity_men, by = c("obesity_year" = "year")) %>%
    select(year, Cancer = Overall, Obesity = Obesity_Pct) %>%
    arrange(year)
  
  df %>%
    mutate(
      Lag = paste0(lag_years, "-year lag"),
      Rolling_Corr = rollapply(
        data = .,
        width = window_size,
        FUN = function(x) cor(x[, "Cancer"], x[, "Obesity"], method = "spearman"),
        by.column = FALSE,
        align = "right",
        fill = NA
      )
    ) %>%
    select(year, Lag, Rolling_Corr)
}

# Get data for each lag
rolling_all <- bind_rows(
  get_rolling_corr(5),
  get_rolling_corr(10),
  get_rolling_corr(15)
)

# Plot
ggplot(rolling_all, aes(x = year, y = Rolling_Corr, color = Lag)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title = "Rolling Spearman Correlations Between Obesity and ORC Incidence",
    subtitle = "Across 5, 10, and 15-Year Lags (5-Year Rolling Window)",
    x = "Year (Cancer Incidence)",
    y = "Spearman Correlation",
    color = "Lag") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Yearly rolling correlation matrix 

# Function to compute rolling correlation and p-value for given lag and window size
get_rolling_corr_matrix <- function(lag_years, window_size = 5) {
  df <- cancer_men %>%
    mutate(obesity_year = year - lag_years) %>%
    inner_join(obesity_men, by = c("obesity_year" = "year")) %>%
    arrange(year) %>%
    select(year, Cancer = Overall, Obesity = Obesity_Pct)
  
  # rolling correlation and p-value by sliding window
  n <- nrow(df)
  results <- lapply(seq(window_size, n), function(i) {
    window_df <- df[(i - window_size + 1):i, ]
    cor_test <- cor.test(window_df$Cancer, window_df$Obesity, method = "spearman", exact = FALSE)
    data.frame(
      Year = df$year[i],    # endpoint year of window
      Lag = factor(paste0(lag_years, "-year lag"), 
                   levels = c("5-year lag", "10-year lag", "15-year lag")),
      Correlation = cor_test$estimate,
      P_value = cor_test$p.value
    )
  })
  
  do.call(rbind, results)
}

# Compute for all lags
cor_matrix_data <- bind_rows(
  get_rolling_corr_matrix(5),
  get_rolling_corr_matrix(10),
  get_rolling_corr_matrix(15)
) %>%
  mutate(Signif = case_when(
    P_value < 0.001 ~ "***",
    P_value < 0.01  ~ "**",
    P_value < 0.05  ~ "*",
    TRUE            ~ ""
  ))

# Plot the matrix heatmap
ggplot(cor_matrix_data, aes(x = factor(Year), y = Lag, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(Correlation, 2), Signif)),
            fontface = ifelse(cor_matrix_data$P_value < 0.05, "bold", "plain"),
            size = 4) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "firebrick", midpoint = 0,
                       limits = c(-1, 1), name = "Spearman\nCorrelation") +
  labs(x = "Year (window end)", y = "Lag", title = "Rolling Spearman Correlations by Year and Lag") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    legend.position = "right")

