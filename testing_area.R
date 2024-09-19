cuminc_result <- tidy(cuminc(Surv(time, statusV) ~ 1, data))

# Extract cumulative incidence for each outcome
event1 <- cuminc_result[cuminc_result$outcome == "1", ]
event2 <- cuminc_result[cuminc_result$outcome == "2", ]
event3 <- cuminc_result[cuminc_result$outcome == "3", ]

# Calculate overall cumulative incidence as the sum of the estimates
overall_data <- data.frame(
  time = event1$time,  # assuming time points are the same for all events
  estimate = event1$estimate + event2$estimate + event3$estimate,
  outcome = "Overall"  # Add an 'outcome' column for the overall line
)

# Add missing columns (like conf.low, conf.high) from cuminc_result to overall_data and fill with NA
missing_cols <- setdiff(names(cuminc_result), names(overall_data))
for (col in missing_cols) {
  overall_data[[col]] <- NA  # Fill missing columns with NA
}

# Combine cumin of cardiac death, breast cancer death and the overall deaths for plotting
combined_data <- rbind(event1, event2, overall_data)




# Smooth curve ------------------------------------------------------------

ggplot(combined_data, aes(x = time, y = estimate, color = outcome)) +
  theme_minimal() +
  geom_line() +
  labs(
    x = "Survival Time (Years)", 
    y = "Cumulative Incidence of CVD Mortality",
    color = "Death Cause") +      # Label for the legend
  ylim(c(0, 1.0)) + 
  xlim(c(0, 20)) +
  scale_color_manual(values = c("1" = "red", "2" = "pink", "Overall" = "black"),
                     labels = c("1" = "Cardiac death", "2" = "Breast Cancer death", "Overall" = "All Causes of Death"))





# With steps --------------------------------------------------------------

ggcuminc(combined_data, aes(x = time, y = estimate, color = outcome)) +
  theme_minimal() +
  add_risktable() +
  labs(
    x = "Survival Time (Years)", 
    y = "Cumulative Incidence of CVD Mortality",
    color = "Death Cause") +      # Label for the legend
  ylim(c(0, 1.0)) + 
  xlim(c(0, 20)) +
  scale_color_manual(values = c("1" = "red", "2" = "pink", "Overall" = "black"),
                     labels = c("1" = "Cardiac death", "2" = "Breast Cancer death", "Overall" = "All Causes of Death")) +
  add_risktable()






# Untitled 2 --------------------------------------------------------------

library(cmprsk)
library(dplyr)
library(ggplot2)

# Assuming your data frame is named `data` with columns:
# - ttdeath: time to death
# - death_cr: event status (1 for cancer death, 2 for CVD death, 3 for other causes)
# - trt: treatment group or other grouping variable

# Step 1: Compute cumulative incidence
cuminc_result <- tidy(cuminc(Surv(time, statusV) ~ 1, data))

# Step 2: Extract and tidy the cumulative incidence results
cuminc_tidy <- do.call(rbind, lapply(cuminc_result, function(x) {
  data.frame(time = x$time,
             estimate = x$est,
             conf.low = x$lower,
             conf.high = x$upper,
             outcome = factor(x$code, levels = c(1, 2, 3), 
                              labels = c("Death from Cancer", "Death from CVD", "Death from Other Causes")))
}))

# Step 3: Calculate overall cumulative incidence
overall_data <- cuminc_tidy %>%
  group_by(time) %>%
  summarise(
    estimate = sum(estimate, na.rm = TRUE),
    conf.low = sum(conf.low, na.rm = TRUE),
    conf.high = sum(conf.high, na.rm = TRUE)
  ) %>%
  mutate(outcome = "Overall")

# Combine the tidy data with the overall data
combined_data <- bind_rows(cuminc_tidy, overall_data)

# Step 4: Plot the cumulative incidence
plot <- ggplot(combined_data, aes(x = time, y = estimate, color = outcome)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    x = "Survival Time (Years)",
    y = "Cumulative Incidence",
    color = "Cause of Death"
  ) +
  scale_color_manual(values = c("Death from Cancer" = "red", 
                                "Death from CVD" = "pink", 
                                "Overall" = "black")) +
  theme_minimal() +
  ylim(c(0, 1)) +
  xlim(c(0, 20))

# Print the plot
print(plot)

