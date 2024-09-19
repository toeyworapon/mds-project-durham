# CEI Analysis for State Data

library(dplyr)
library(readr)


file_list <- c(
  "CEI2002_ByCompany_Scores_REV.csv",
  "CEI2003_ByCompany_Scores_REV.csv",
  "CEI2004_ByCompany_Scores_REV.csv",
  "CEI2005_ByCompany_Scores_REV.csv",
  "CEI2006_ByCompany_Scores_REV.csv",
  "CEI2008_ByCompany_Scores_REV.csv",
  "CEI2009_ByCompany_Scores_REV.csv",
  "CEI2010_ByCompany_Scores_REV.csv",
  "CEI2011_ByCompany_Scores_REV.csv",
  "CEI2012_ByCompany_Scores_REV.csv",
  "CEI2013_ByCompany_Scores_REV.csv",
  "CEI2014_ByCompany_Scores_REV.csv",
  "CEI2015_ByCompany_Scores_REV.csv",
  "CEI2016_ByCompany_Scores_REV.csv",
  "CEI2017_ByCompany_Scores_REV.csv",
  "CEI2018_ByCompany_Scores_REV.csv",
  "CEI2019_ByCompany_Scores_REV.csv",
  "CEI2020_ByCompany_Scores_REV.csv",
  "CEI2021_ByCompany_Scores_REV.csv",
  "CEI2022_ByCompany_Scores_REV.csv",
  "CEI2023_ByCompany_Scores_REV.csv"
)

# Function to read each file and add a Year column
read_and_label <- function(file) {
  # Extract the year from the filename using a regular expression
  year <- as.numeric(gsub("\\D", "", basename(file)))  
  data <- read_csv(file)
  data$Year <- year
  return(data)
}

# Read all files and combine them into one dataframe
all_cei_data <- bind_rows(lapply(file_list, read_and_label))

# Check the combined data
head(all_cei_data)



library(dplyr)
library(ggplot2)

# Create a region mapping for U.S. states
state_region_mapping <- c(
  'CT' = 'Northeast', 'ME' = 'Northeast', 'MA' = 'Northeast', 'NH' = 'Northeast', 'RI' = 'Northeast', 'VT' = 'Northeast', 
  'NJ' = 'Northeast', 'NY' = 'Northeast', 'PA' = 'Northeast',
  'IL' = 'Midwest', 'IN' = 'Midwest', 'MI' = 'Midwest', 'OH' = 'Midwest', 'WI' = 'Midwest', 'IA' = 'Midwest', 
  'KS' = 'Midwest', 'MN' = 'Midwest', 'MO' = 'Midwest', 'NE' = 'Midwest', 'ND' = 'Midwest', 'SD' = 'Midwest',
  'DE' = 'South', 'FL' = 'South', 'GA' = 'South', 'MD' = 'South', 'NC' = 'South', 'SC' = 'South', 'VA' = 'South', 
  'WV' = 'South', 'AL' = 'South', 'KY' = 'South', 'MS' = 'South', 'TN' = 'South', 'AR' = 'South', 'LA' = 'South', 
  'OK' = 'South', 'TX' = 'South', 'DC' = 'South',
  'AZ' = 'West', 'CO' = 'West', 'ID' = 'West', 'MT' = 'West', 'NV' = 'West', 'NM' = 'West', 'UT' = 'West', 'WY' = 'West',
  'AK' = 'West', 'CA' = 'West', 'HI' = 'West', 'OR' = 'West', 'WA' = 'West'
)

# Add the Region column to the dataset by mapping State to Region
all_cei_data$Region <- state_region_mapping[all_cei_data$State]

# Clean the data to keep only the columns we need (Year, Region, and CEI Rating)
all_cei_data_clean <- all_cei_data %>%
  filter(!is.na(`CEI Rating`), !is.na(Region)) %>%
  group_by(Year, Region) %>%
  summarise(mean_cei_rating = mean(`CEI Rating`, na.rm = TRUE))

# 1. Visualize the overall trend before modeling
ggplot(all_cei_data_clean, aes(x = Year, y = mean_cei_rating, color = Region)) +
  geom_line(size = 1) +
  labs(title = "CEI Rating Trends by Region Over Time (2002-2023)", x = "Year", y = "Mean CEI Rating") +
  theme_minimal()

# 2. Time Series Linear Regression Model
model <- lm(mean_cei_rating ~ Year * Region, data = all_cei_data_clean)

# 3. Summary of the model to interpret coefficients
summary(model)


# Fit a second-degree polynomial regression model to capture non-linear trends
poly_model <- lm(mean_cei_rating ~ poly(Year, 2) * Region, data = all_cei_data_clean)

# Summary of the polynomial regression model
summary(poly_model)

# Plot the fitted polynomial trend lines along with the actual data
all_cei_data_clean$predicted_poly <- predict(poly_model)

ggplot(all_cei_data_clean, aes(x = Year, y = mean_cei_rating, color = Region)) +
  geom_line(size = 1) +
  geom_line(aes(y = predicted_poly), linetype = "dashed", size = 1) +
  labs(title = "Polynomial Regression Trends of CEI Ratings by Region (2002-2023)", x = "Year", y = "Mean CEI Rating") +
  theme_minimal()
ggsave("polynomialCEIregion.svg", width = 10, height = 8)



# Create boxplots to visualize the distribution of CEI Ratings for each region over time
ggplot(all_cei_data, aes(x = factor(Year), y = `CEI Rating`, fill = Region)) +
  geom_boxplot() +
  facet_wrap(~ Region) +
  labs(title = "Distribution of CEI Ratings by Region Over Time (2002-2023)", x = "Year", y = "CEI Rating") +
  theme_minimal()



library(mgcv)

# Fit a Generalized Additive Model (GAM) to capture smooth trends
gam_model <- gam(mean_cei_rating ~ s(Year) + Region, data = all_cei_data_clean)

# Summary of the GAM model
summary(gam_model)

# Predict values from the GAM model
all_cei_data_clean$predicted_gam <- predict(gam_model)

# Plot the actual vs predicted values using the GAM model
ggplot(all_cei_data_clean, aes(x = Year, y = mean_cei_rating, color = Region)) +
  geom_line(size = 1) +
  geom_line(aes(y = predicted_gam), linetype = "dashed", size = 1) +
  labs(title = "GAM Model: CEI Rating Trends by Region (2002-2023)", x = "Year", y = "Mean CEI Rating") +
  theme_minimal()

# ARIMA Forecast

library(forecast)

# ARIMA requires a time series object
arima_forecasts <- list()

for(region in unique(all_cei_data_clean$Region)) {
  
  # Filter data for the specific region
  region_data <- all_cei_data_clean %>%
    filter(Region == region) %>%
    arrange(Year)
  
  # Convert the data to a time series object
  cei_ts <- ts(region_data$mean_cei_rating, start = min(region_data$Year), frequency = 1)
  
  # Fit an ARIMA model
  fit_arima <- auto.arima(cei_ts)
  
  # Forecast for the next 10 years
  forecast_arima <- forecast(fit_arima, h = 7)
  
  # Store the results
  arima_forecasts[[region]] <- forecast_arima
}

# Plot the forecasts for each region

par(mfrow = c(2, 2)) # Arrange the plots in a 2x2 grid
for(region in names(arima_forecasts)) {
  plot(arima_forecasts[[region]], main = paste("ARIMA Forecast for", region), xlab = "Year", ylab = "CEI Rating")
}



# ETS Time Series Forecast

# Fit ETS models for each region
ets_forecasts <- list()

for(region in unique(all_cei_data_clean$Region)) {
  
  # Filter data for the specific region
  region_data <- all_cei_data_clean %>%
    filter(Region == region) %>%
    arrange(Year)
  
  # Convert the data to a time series object
  cei_ts <- ts(region_data$mean_cei_rating, start = min(region_data$Year), frequency = 1)
  
  # Fit an ETS model
  fit_ets <- ets(cei_ts)
  
  # Forecast for the next 10 years
  forecast_ets <- forecast(fit_ets, h = 7)
  
  # Store the results
  ets_forecasts[[region]] <- forecast_ets
}

# Plot the ETS forecasts for each region
par(mfrow = c(2, 2)) # Arrange the plots in a 2x2 grid
for(region in names(ets_forecasts)) {
  plot(ets_forecasts[[region]], main = paste("ETS Forecast for", region), xlab = "Year", ylab = "CEI Rating")
}


# AVERAGE CEI

# Load necessary libraries
library(usmap)
library(dplyr)
library(ggplot2)

# Calculate the average CEI Rating for each state across all years
state_avg_cei <- all_cei_data %>%
  group_by(State) %>%
  summarise(avg_cei_rating = mean(`CEI Rating`, na.rm = TRUE))

# Map state abbreviations to full state names
state_avg_cei$state <- state.name[match(state_avg_cei$State, state.abb)]

# Plot heatmap using usmap
plot_usmap(data = state_avg_cei, values = "avg_cei_rating", lines = "black") +
  scale_fill_viridis_c(na.value = "white", name = "Avg CEI Rating") +
  labs(title = "Average CEI Ratings by State (2002-2023)") +
  theme(legend.position = "right")
ggsave("Avg_CEI_StateScore.svg", width = 10, height = 6)

# Load necessary libraries
library(usmap)
library(dplyr)
library(ggplot2)

# Calculate the average CEI Rating for each state across all years
state_avg_cei <- all_cei_data %>%
  group_by(State) %>%
  summarise(avg_cei_rating = mean(`CEI Rating`, na.rm = TRUE))

# Map state abbreviations to full state names
state_avg_cei$state <- state.name[match(state_avg_cei$State, state.abb)]

# Plot heatmap using usmap
plot_usmap(data = state_avg_cei, values = "avg_cei_rating", lines = "black") +
  scale_fill_continuous(low = "yellow", high = "red", name = "Avg CEI Rating") +
  labs(title = "Average CEI Ratings by State (2002-2023)") +
  theme(legend.position = "right")

library(forecast)
library(dplyr)

# Split the data into training (2002-2018) and test sets (2019-2023)
training_data <- all_cei_data_clean %>%
  filter(Year <= 2018)

test_data <- all_cei_data_clean %>%
  filter(Year > 2018)

# Fit ARIMA model on the training set for each region
arima_models <- list()
forecasts <- list()

for(region in unique(training_data$Region)) {
  # Filter data for the specific region
  region_train_data <- training_data %>%
    filter(Region == region) %>%
    arrange(Year)
  
  # Convert the data to a time series object
  cei_ts <- ts(region_train_data$mean_cei_rating, start = min(region_train_data$Year), frequency = 1)
  
  # Fit an ARIMA model
  fit_arima <- auto.arima(cei_ts)
  
  # Forecast for the test set period (2019-2023)
  forecast_arima <- forecast(fit_arima, h = length(test_data$Year))
  
  # Store the model and forecast
  arima_models[[region]] <- fit_arima
  forecasts[[region]] <- forecast_arima
}

# Function to calculate forecast accuracy for each region
calculate_forecast_accuracy <- function(region) {
  # Get actual and predicted values for the test set
  actual_values <- test_data %>%
    filter(Region == region) %>%
    pull(mean_cei_rating)
  
  predicted_values <- forecasts[[region]]$mean
  
  # Calculate accuracy metrics
  mae <- mean(abs(actual_values - predicted_values))
  rmse <- sqrt(mean((actual_values - predicted_values)^2))
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  
  return(data.frame(Region = region, MAE = mae, RMSE = rmse, MAPE = mape))
}

# Calculate accuracy for all regions
accuracy_results <- do.call(rbind, lapply(unique(test_data$Region), calculate_forecast_accuracy))

# View accuracy results
print(accuracy_results)




# Plot residuals and perform diagnostics for each region's ARIMA model
for(region in names(arima_models)) {
  # Get the residuals for the ARIMA model
  residuals_arima <- residuals(arima_models[[region]])
  
  # Plot the residuals
  par(mfrow = c(2, 2)) # 2x2 plot grid
  plot(residuals_arima, main = paste("Residuals for", region))
  Acf(residuals_arima, main = paste("ACF of Residuals for", region)) # Check for autocorrelation
  hist(residuals_arima, main = paste("Histogram of Residuals for", region), xlab = "Residuals")
  qqnorm(residuals_arima)
  qqline(residuals_arima)
}
