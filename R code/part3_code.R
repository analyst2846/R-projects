#| Statistical Modeling - Project Part 3

# -------------------------------
#| Load necessary libraries
# -------------------------------

packages <- c('nlme', 
              'dplyr',
              'ggplot2',
              'psych')

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
#|    1.1 Data Preprocessing
#|    1.2 Missing Values
#|    1.3 Distinct Stations
#|    1.4 Number of Observations
#|    1.5 Descriptive Statistics
#| 2. Model 1
#|    2.1 a)
#|    2.2 b)
#| 3. Model 2
#|    3.1 a)
#|    3.2 b)
#|    3.3 c)
#| 4. Model 3
#|    4.1 a)
#|    4.2 b)
#|    4.3 c)
# -------------------------------

# -------------------------------
#| 0. EDA
# -------------------------------
#| 0.1 Data Preprocessing
# -------------------------------

# Data
bixi_data <- read.csv("bixi3_grp5.csv")

# Preprocess the data
bixi_data <- bixi_data %>%
  rename(Month = mm, Day = dd) %>% # Rename columns
  mutate(
    # Factorize categorical variables
    Month = factor(Month),
    Day = factor(Day),
    arrondissement = factor(arrondissement),
    wday = factor(wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    
    # Create weekend variable (1 for Saturday/Sunday, 0 otherwise)
    weekend = ifelse(wday %in% c("Saturday", "Sunday"), 1, 0),
  ) %>%
  select(-Day) # Remove the 'Day' column

# -------------------------------
#| 0.2 Missing Values
# -------------------------------

#| There are no missing values in the dataset. 

colSums(is.na(bixi_data))

# -------------------------------
#| 0.3 Distinct Stations
# -------------------------------

#| There are 100 unique stations. 

unique(bixi_data$station)

# -------------------------------
#| 0.4 Number of Observations
# -------------------------------

#| The number of observations corresponds to the number of rows, 1000.

nrow(bixi_data)

# -------------------------------
#| 0.5 Descriptive Statistics
# -------------------------------

station_counts <- bixi_data %>%
  count(station)

summary(station_counts$n)


# -------------------------------
#| 1. Model 1
# -------------------------------
#| 1.1 a)
# -------------------------------
#| Fit the linear regression model assuming independent observations

model1 <- gls(dur ~ arrondissement + weekend, data = bixi_data)
summary(model1)

# -------------------------------
#| 1.2 b) 
# -------------------------------
#| Add fixed effects for station

model1b <- gls(dur ~ arrondissement + weekend + as.factor(station), data = bixi_data)


# Check if stations are uniquely assigned to boroughs
station_borough_table <- bixi_data %>%
  group_by(station, arrondissement) %>%
  summarise(count = n(), .groups = "drop")

# Count unique boroughs per station
unique_boroughs_per_station <- station_borough_table %>%
  group_by(station) %>%
  summarise(unique_boroughs = n_distinct(arrondissement))

# View stations with more than one unique borough
unique_boroughs_per_station %>% filter(unique_boroughs > 1) # Should return an empty result



# -------------------------------
#| 2. Model 2
# -------------------------------
#| 2.1 a)
# -------------------------------
#|Model 2: Fit a linear regression model with random intercept at the station level

model2 <- lme(dur ~ arrondissement + weekend, 
              random = ~ 1 | station, 
              data = bixi_data, 
              method = "REML")
summary(model2)


# Part 2.a: Calculate within-station correlation
variance_components <- VarCorr(model2)
station_variance <- as.numeric(variance_components[1, "Variance"])
residual_variance <- as.numeric(variance_components[2, "Variance"])

# Calculate within-station correlation
within_station_correlation <- station_variance / (station_variance + residual_variance)

cat("Random Intercept Variance:", station_variance, "\n")
cat("Residual Variance:", residual_variance, "\n")
cat("Within Station Correlation:", within_station_correlation, "\n")

# -------------------------------
#| 2.2 b)
# -------------------------------

anova(model1, model2)  # Compare models


# -------------------------------
#| 2.3 c)
# -------------------------------

summary(model2)

# Fit the full and reduced models using ML
full_model_ml <- lme(fixed = dur ~ arrondissement + weekend, 
                     random = ~ 1 | station, 
                     data = bixi_data, 
                     method = "ML")

reduced_model_ml <- lme(fixed = dur ~ weekend, 
                        random = ~ 1 | station, 
                        data = bixi_data, 
                        method = "ML")

# Perform a likelihood ratio test between the ML-fitted models
anova_result <- anova(reduced_model_ml, full_model_ml)  # Compare reduced and full models
print(anova_result)


# -------------------------------
#| 3. Model 3
# -------------------------------
#| 3.1 a)
# -------------------------------

# Fit the mixed effects model

model3 <- lme(
  fixed = dur ~ arrondissement + weekend,
  random = list(station = pdDiag(~ 1 + weekend)),
  data = bixi_data,
  method = "REML"
)

# Extract fixed effect for weekend
fixed_effect_weekend <- fixef(model3)["weekend"]

# Extract random effects
random_effects <- ranef(model3)

# Calculate station-level predictions for the weekend effect
station_level_predictions <- setNames(fixed_effect_weekend + random_effects$weekend, rownames(random_effects))

# Identify the station with the greatest weekend effect
max_station <- names(which.max(station_level_predictions))
max_effect <- max(station_level_predictions)

# Print the results
cat("Station with the greatest weekend effect:", max_station, "\n")
cat("Increase in average trip duration on weekends:", max_effect, "minutes\n")

# -------------------------------
#| 3.2 b)
# -------------------------------
#| Perform a likelihood ratio test to determine if there is significant variation
#| in the weekend effect across stations

model3_no_weekend <- lme(
  fixed = dur ~ arrondissement + weekend,
  random = ~ 1 | station,
  data = bixi_data,
  method = "REML"
)

anova(model3_no_weekend, model3)

# -------------------------------
#| 3.3 c)
# -------------------------------'

# Extract variance components
VarCov_components <- getVarCov(model3)

# Extract specific components
station_intercept_var <- VarCov_components[1, 1]  # Random intercept variance
station_weekend_var <- VarCov_components[2, 2]  # Random slope variance for weekend
residual_var <- as.numeric(VarCorr(model3)["Residual", "Variance"])          # Residual variance

# Compute correlations
# Same station, both weekdays
cor_weekday_same_station <- station_intercept_var / 
  (station_intercept_var + residual_var)

# Same station, both weekends
cor_weekend_same_station <- (station_intercept_var + station_weekend_var) / 
  (station_intercept_var + station_weekend_var + residual_var)


# Print the results
cat("Correlation between two weekday observations leaving from the same station:", 
    cor_weekday_same_station, "\n")
cat("Correlation between two weekend observations leaving from the same station:", 
    cor_weekend_same_station, "\n")
