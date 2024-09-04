


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Simulate data
data <- data.frame(
  group = rep(c("Group1", "Group2", "Group3", "Group4"), each = 10),
  observed_deaths = rpois(40, lambda = 5),
  expected_deaths = rpois(40, lambda = 5) + 1
)

# Fit Poisson regression model with group as a factor
fit_full <- glm(observed_deaths ~ group + offset(log(expected_deaths)), 
                family = poisson(link = "log"), data = data)

# Fit null model without group factor
fit_null <- glm(observed_deaths ~ 1 + offset(log(expected_deaths)), 
                family = poisson(link = "log"), data = data)

# Perform likelihood ratio test
lrt_result <- anova(fit_null, fit_full, test = "LRT")
print(lrt_result)

# Extract p-value from the LRT result
p_value <- lrt_result$`Pr(>Chi)`[2]
cat("P-value for heterogeneity:", p_value, "\n")

# If the p-value is significant, it suggests heterogeneity across groups



# Plot --------------------------------------------------------------------
# Simulate data
data <- data.frame(
  group = rep(c("Asian American", "Black", "Latina", "White"), each = 4 * 4),
  treatment = rep(c("Overall", "Surgery only", "Chemotherapy with surgery", "Radiotherapy with surgery"), times = 4 * 4),
  stage = rep(c("Localized", "Regional", "Distant", "DS"), times = 4 * 4),
  SMR = runif(64, 0.5, 3),  # Simulated SMR values
  lower_CI = runif(64, 0.3, 2.5),  # Simulated lower CI values
  upper_CI = runif(64, 0.7, 3.5)  # Simulated upper CI values
)

# Ensure CI is ordered correctly
data <- data %>%
  mutate(lower_CI = pmin(SMR, lower_CI),
         upper_CI = pmax(SMR, upper_CI))
# Add heterogeneity analysis result to the plot
p_value_text <- paste("P-value for heterogeneity: ", round(p_value, 4))

p <- ggplot(data, aes(x = SMR, y = interaction(treatment, stage))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ group, ncol = 4) +
  labs(title = "Standardized Mortality Ratios by Group",
       x = "Standardized Mortality Ratio (SMR)",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"),
    legend.position = "none"
  )

# Add p-value text annotation
p + geom_text(aes(label = sprintf("%.2f (%.2f - %.2f)", SMR, lower_CI, upper_CI)), 
              hjust = -0.1, size = 3, nudge_y = 0.25) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = unit(c(1, 4, 1, 1), "lines")) +
  annotate("text", x = Inf, y = Inf, label = p_value_text, hjust = 1.1, vjust = 2, size = 4, color = "black")
