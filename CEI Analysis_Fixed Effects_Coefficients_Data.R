# FIXED EFFECT AND COEFFICIENTS ANALYSIS


# COEFFICIENTS

# Load necessary libraries
library(plm)
library(dplyr)
library(ggplot2)

# Assuming 'panel_data' has been created and contains 'City', 'Year', and 'CEI Rating' columns
# Ensure there are no issues with column names
colnames(panel_data) <- gsub(" ", "_", colnames(panel_data))  # Replace any spaces with underscores

panel_data_baseline <- panel_data %>% filter(Year != 2002)

panel_data_baseline <- pdata.frame(panel_data_baseline, index = c("City", "Year"))


# Regress CEI Rating on Year with fixed effects, using 2002 as the baseline
fixed_effects_model <- plm(CEI_Rating ~ Year, 
                           data = panel_data_baseline, 
                           model = "within")

summary(fixed_effects_model)

# Plotting the Fixed Effects Model Coefficients
coefficients <- coef(fixed_effects_model)
year_labels <- names(coefficients)

# Convert coefficients to a data frame for easier plotting
coeff_df <- data.frame(
  Year = as.numeric(gsub("Year", "", year_labels)),
  Coefficient = as.numeric(coefficients)
)

# Plot the year coefficients
ggplot(coeff_df, aes(x = Year, y = Coefficient)) +
  geom_line() +
  geom_point() +
  labs(title = "Fixed Effects Model: Coefficients Over Time",
       x = "Year",
       y = "Coefficient (Relative to 2010)") +
  theme_minimal()


# Extract the coefficients from the model
coef_df <- summary(fixed_effects_model)$coefficients

# Print coefficients
print(coef_df)


# Extract the F-statistic
f_stat <- summary(fixed_effects_model)$fstatistic
print(f_stat)


# Create a data frame for plotting the coefficients
coef_df_plot <- as.data.frame(coef_df)
coef_df_plot$Year <- rownames(coef_df)

# Plot the coefficients
ggplot(coef_df_plot, aes(x = Year, y = Estimate)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  theme_minimal() +
  labs(title = "CEI Rating Coefficients Relative to 2010", x = "Year", y = "Coefficient (Relative to 2010)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Line plot of coefficients over time
ggplot(coef_df_plot, aes(x = Year, y = Estimate, group = 1)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(title = "CEI Rating Coefficients Over Time (Relative to 2010)", x = "Year", y = "Coefficient Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# FIXED EFFECT REGRESSION


# Load necessary libraries
library(plm)
library(dplyr)
library(ggplot2)
library(broom)

panel_data <- pdata.frame(panel_data, index = c("City", "Year"))

# Perform Fixed Effects Regression with interaction term for City
fixed_effects_city_model <- plm(CEI_Rating ~ Year, data = panel_data, model = "within", effect = "individual")

summary(fixed_effects_city_model)

# Use 'fixef' function to extract the fixed effects for each city
city_effects <- fixef(fixed_effects_city_model)
city_effects_df <- as.data.frame(city_effects)
city_effects_df$City <- rownames(city_effects_df)
colnames(city_effects_df)[1] <- "City_Effect"

ggplot(city_effects_df, aes(x = reorder(City, City_Effect), y = City_Effect)) +
  geom_col(fill = "steelblue") +
  labs(title = "City Fixed Effects from CEI Ratings Model",
       x = "City",
       y = "Fixed Effect Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
