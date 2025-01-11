# -------------------------------
#| Load necessary libraries
# -------------------------------

# List of required packages
packages <- c('dplyr', 
              'lubridate', 
              'ggplot2', 
              'car', 
              'broom', 
              'knitr', 
              'kableExtra', 
              'readr',
              'gridExtra')

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

# -------------------------------
#| Table of Contents

#| 1. EDA
#|    1.1 Data Cleaning
#|    1.2 Generating Plots
#| 2. Finding Base Model
#|    2.1 Data Preprocessing
#|    2.2 Model Fitting
#|    2.3 Model Comparison Using BIC
#|    2.4 Overdispersion Check
#|    2.5 Outlier Detection
#|    2.6 Refit Model without Outliers
#|    2.7 Coefficient Comparison
#|    2.8 Decision based on Coefficient Comparison
#| 3. Analysis Question 1
#|    3.1 1a)
#|    3.2 1b)
#|    3.3 1c)
#|    3.4 1d)

# -------------------------------


# -------------------------------
# 1. EDA
# -------------------------------

# Load the dataset
bixi_data <- read.csv("bixi2_grp5.csv")

# Get the proportion of AM trips
bixi_data$AM_proportion <- bixi_data$AM/bixi_data$tot

# Create temperature bins with extended range to avoid NA's
bixi_data$temp_bin <- cut(bixi_data$temp, 
                          breaks = seq(min(bixi_data$temp, na.rm = TRUE) - 1, 
                                       max(bixi_data$temp, na.rm = TRUE) + 1, by = 4), 
                          include.lowest = TRUE)

bixi_data$precip_bin <- cut(bixi_data$precip, 
                            breaks = c(-Inf, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, Inf), 
                            labels = c("0", "(0,2]", "(2,4]", "(4,6]", "(6,8]", "(8,10]", 
                                       "(10,12]", "(12,14]", "(14,16]", "(16,18]", "(18,20]", "(20,inf)"),
                            include.lowest = TRUE)

# Change mm to integers
bixi_data$mm <- as.integer(bixi_data$mm)
bixi_data$mm <- factor(bixi_data$mm, levels = c(5, 6, 7, 8, 9, 10), labels = c("May", "June", "July", "August", "September", "October"))

# Count unique stations in each arrondissement
station_counts <- bixi_data %>%
  group_by(arrondissement) %>%
  summarise(unique_stations = n_distinct(station))

# Ensure the weekdays are ordered Monday to Sunday
bixi_data$wday <- factor(bixi_data$wday, 
                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# -------------------------------
# 1.2 Generating Plots 
# -------------------------------

# Violin plot for days of the week
violin_plot <- ggplot(bixi_data, aes(x = wday, y = AM_proportion, fill = wday)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  
  stat_summary(fun = "mean", geom = "point", color = "red", shape = 18, size = 3) + 
  labs(title = "Proportion of AM Trips during the Week", 
       x = "Day of the Week", 
       y = "Proportion of AM Trips") +
  ylim(0, NA) +  
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))  

# Violin plot for months
violin_plot_month <- ggplot(bixi_data, aes(x = mm, y = AM_proportion, fill = mm)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  
  geom_jitter(width = 0.2, alpha = 0.5, color = "black", size = 1) +  
  stat_summary(fun = "mean", geom = "point", color = "red", shape = 18, size = 3) + 
  labs(title = "Proportion of AM Trips by Month", 
       x = "Month", 
       y = "Proportion of AM Trips") +
  ylim(0, NA) +  # Sets the y-axis to start at 0
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))  

# Arranging plots next to each other
arranged_plot_1 <- grid.arrange(violin_plot, violin_plot_month, ncol = 2)

# Saving plot
ggsave("proportion_month_week.png", plot = arranged_plot_1, width = 12, height = 5, units = "in", dpi = 300)

# Histogram with density
hist_plot <- ggplot(bixi_data, aes(x = AM_proportion)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", color = "black") +
  geom_density(adjust = 1, alpha = 0.5, fill = "skyblue") +
  geom_vline(xintercept = quantile(bixi_data$AM_proportion, probs = 0.95), 
             col = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = quantile(bixi_data$AM_proportion, probs = 0.99), 
             col = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Morning Trip Proportions",
       x = "Proportion of Morning Trips",
       y = "Density") +
  annotate("text", x = quantile(bixi_data$AM_proportion, probs = 0.95), 
           y = max(density(bixi_data$AM_proportion)$y) * 0.9, 
           label = "95% Quantile", color = "red", hjust = 0) +
  annotate("text", x = quantile(bixi_data$AM_proportion, probs = 0.99), 
           y = max(density(bixi_data$AM_proportion)$y) * 0.8, 
           label = "99% Quantile", color = "blue", hjust = 0) +
  theme_minimal()

# Bar plot of precipitation bins
bar_plot <- ggplot(bixi_data, aes(x = precip_bin)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Binned Precipitation Levels",
       x = "Precipitation Bin (mm)",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arranging the two plots next to each other 
arranged_plot_2 <- grid.arrange(hist_plot, bar_plot, ncol = 2)

# Saving the plot
ggsave("precip_proportion_dist.png", plot = arranged_plot_2, width = 12, height = 4.5, units = "in", dpi = 300)

# Plot for proportion by arrondissement
borough_plot <- ggplot(bixi_data, aes(x = arrondissement, y = AM_proportion, fill = arrondissement)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  
  geom_jitter(width = 0.2, alpha = 0.5, color = "black", size = 1) +  
  stat_summary(fun = "mean", geom = "point", color = "red", shape = 18, size = 3) +  
  labs(title = "Proportion of AM Trips by Borough", 
       x = "Borough", 
       y = "Proportion of AM Trips") +
  ylim(0, NA) +  # Sets the y-axis to start at 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")  

# Saving plot
ggsave("proportion_AM_trips_borough.png", plot = borough_plot, width = 10, height = 6, units = "in", dpi = 300)

# Count the number of entries for each borough
borough_counts <- bixi_data %>%
  group_by(arrondissement) %>%
  summarise(Number_of_Entries = n()) %>%
  arrange(Number_of_Entries)  

# Display the table
borough_counts %>%
  kable(col.names = c("Borough", "Number of Entries"), 
        caption = "Number of Entries per Borough") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


# -------------------------------
# 2. Finding Base Model
# -------------------------------
# 2.1 Data Preprocessing
# -------------------------------

# Preprocess the data
bixi_data <- bixi_data %>%
  rename(Month = mm, Day = dd) %>% # Rename columns
  mutate(
    # Factorize categorical variables
    Month = factor(Month),
    Day = factor(Day),
    arrondissement = factor(arrondissement),
    wday = factor(wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    
    # Convert precipitation to a binary factor (0 = no rain, 1 = rain)
    precip = as.numeric(precip),
    precip = factor(ifelse(precip > 0, 1, 0)),
    
    # Create weekend variable (1 for Saturday/Sunday, 0 otherwise)
    weekend = factor(ifelse(wday %in% c("Saturday", "Sunday"), 1, 0)),
    
    # Calculate the proportion of AM trips
    AM_proportion = AM / tot
  ) %>%
  select(-Day) # Remove the 'Day' column

# Display the first few rows of the preprocessed data
head(bixi_data)


# -------------------------------
# 2.2 Model Fitting
# -------------------------------

# Fit the logistic regression model with 'weekend' as a covariate
base_model <- glm(cbind(AM, tot - AM) ~ Month + arrondissement + temp + precip +
                    weekend + weekend:precip + weekend:temp + temp:precip,
                  family = binomial(link = "logit"), data = bixi_data)

# Fit the logistic regression model with 'wday' as a covariate
base_model_wday <- glm(cbind(AM, tot - AM) ~ Month + arrondissement + temp + precip +
                         wday + wday:precip + wday:temp + temp:precip,
                       family = binomial(link = "logit"), data = bixi_data)

# Display summaries of both models
summary(base_model)
summary(base_model_wday)


# -------------------------------
# 2.3 Model Comparison Using BIC
# -------------------------------

# Calculate BIC for both models
bic_weekend <- BIC(base_model)
bic_wday <- BIC(base_model_wday)

# Print BIC values and compare
cat("BIC for model with 'weekend':", bic_weekend, "\n")
cat("BIC for model with 'wday':", bic_wday, "\n")

# Determine the preferred model based on BIC
if (bic_wday < bic_weekend) {
  cat("The model with 'wday' is preferred based on BIC.\n")
} else {
  cat("The model with 'weekend' is preferred based on BIC.\n")
}


# -------------------------------
# 2.4 Overdispersion Check
# -------------------------------

check_overdispersion <- function(model) {
  # Calculate the dispersion value
  dispersion_value <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  cat("Dispersion value:", round(dispersion_value, 3), "\n")
  
  # Assess overdispersion
  if (dispersion_value > 1) {
    cat("Overdispersion detected (Dispersion value > 1).\n")
  } else {
    cat("No overdispersion detected (Dispersion value <= 1).\n")
  }
  
  # Plot diagnostic plots
  par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))
  plot(model)
}

# Check overdispersion for the base model
check_overdispersion(base_model)


# -------------------------------
# 2.5 Outlier Detection
# -------------------------------

# Calculate standardized Pearson residuals
pearson_residuals <- residuals(base_model, type = "pearson")
standardized_residuals <- pearson_residuals / sqrt(1 - hatvalues(base_model))

# Identify outliers using a threshold of |residual| > 3
outlier_threshold <- 3
influential_points <- which(abs(standardized_residuals) > outlier_threshold)

# Print the number of outliers identified
cat("Number of influential points identified:", length(influential_points), "\n")


# -------------------------------
# 2.6 Refit Model Without Outliers
# -------------------------------

# Create a cleaned dataset without outliers
bixi_data_clean <- bixi_data[-influential_points, ]

# Refit the model without influential points
base_model_clean <- glm(cbind(AM, tot - AM) ~ Month + arrondissement + temp + precip +
                          weekend + weekend:precip + weekend:temp + temp:precip,
                        family = binomial(link = "logit"), data = bixi_data_clean)

# Display the summary of the cleaned model
summary(base_model_clean)

# Check overdispersion for the cleaned model
check_overdispersion(base_model_clean)


# -------------------------------
# 2.7 Coefficient Comparison
# -------------------------------

# Extract coefficients and p-values for both models
coef_base <- tidy(base_model) %>%
  select(term, estimate, p.value) %>%
  rename(estimate_base = estimate, p.value_base = p.value)

coef_clean <- tidy(base_model_clean) %>%
  select(term, estimate, p.value) %>%
  rename(estimate_clean = estimate, p.value_clean = p.value)

# Merge results for comparison
comparison <- full_join(coef_base, coef_clean, by = "term") %>%
  mutate(
    significance_status = case_when(
      p.value_clean < 0.05 & p.value_base < 0.05 ~ "Both significant",
      p.value_clean >= 0.05 & p.value_base >= 0.05 ~ "Both non-significant",
      TRUE ~ "Inconsistency"
    )
  )

# Display the comparison table
comparison_table <- comparison %>%
  kable("html", caption = "Comparison of Coefficient Estimates, P-values, and Significance Status") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

comparison_table


# -------------------------------
# 2.8 Decision Based on Coefficient Comparison
# -------------------------------

cat("Based on the coefficient comparison, we continue with the initial model as there are no drastic changes in interpretation after removing outliers.\n")


# -------------------------------
# 3. Analysis of Question 1
# -------------------------------
# 3.1 1a)
# -------------------------------

#| We can use a Wald Test by assessing the pvalues of the coefficients.

summary(base_model)

#| Or we can continue with LRT test to see if including arrondissement would 
#| change the mean in its different levels

# Analysis of Deviance (Likelihood Ratio Test)
anova_base <- anova(base_model, test = "Chisq")

# Test result for the "arrondissement" variable
anova_base

# -------------------------------
# 3.2 1b)
# -------------------------------

summary(base_model)


# -------------------------------
# 3.3 1c)
# -------------------------------

summary(base_model)


# -------------------------------
# 3.4 1d)
# -------------------------------
#| Perform wald test based on summary(base_model)

summary(base_model)

# Extract the p-value for the interaction term 'temp:weekend'
p_value <- summary(base_model)$coefficients["temp:weekend1", "Pr(>|z|)"]

#| Check if the p-value is less than the significance level (α = 0.01)

if (p_value < 0.01) {
  cat("The impact of temperature on the odds of a morning departure significantly differs on weekends compared to weekdays (p-value:", p_value, ").\n")
} else {
  cat("There is no significant difference in the impact of temperature on the odds of a morning departure between weekends and weekdays (p-value:", p_value, ").\n")
}

#| Perform LRT test

# Fit the reduced model without the 'temp:weekend' interaction term
reduced_model <- glm(cbind(AM, tot - AM) ~ Month + arrondissement + temp + precip +
                       weekend + weekend:precip + temp:precip,
                     family = binomial(link = "logit"), data = bixi_data)

# Perform the Likelihood Ratio Test
lrt_result <- anova(reduced_model, base_model, test = "LRT")

# Display the result
print(lrt_result)

# Extract the p-value
lrt_p_value <- lrt_result$`Pr(>Chi)`[2]

#| Check the significance at α = 0.01

if (lrt_p_value < 0.01) {
  cat("The impact of temperature on the odds of a morning departure significantly differs on weekends compared to weekdays (p-value:", lrt_p_value, ").\n")
} else {
  cat("There is no significant difference in the impact of temperature on the odds of a morning departure between weekends and weekdays (p-value:", lrt_p_value, ").\n")
}

# -------------------------------
# -------------------------------
# -------------------------------

