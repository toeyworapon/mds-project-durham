# ARIMA FOR NATIONAL CEI RATINGS

install.packages("tseries")

# Load necessary libraries
library(forecast)
library(ggplot2)
library(dplyr)
library(readr)
library(tseries)


# Read and combine all data files

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
  year <- as.numeric(gsub("\\D", "", basename(file)))  # Extract the year from the filename
  data <- read_csv(file)
  data$Year <- year
  return(data)
}

# Read all files and combine them into one dataframe
all_cei_data <- bind_rows(lapply(file_list, read_and_label))

# Create a region mapping for U.S. states
state_region_mapping <- c(
  'CT' = 'Northeast', 'ME' = 'Northeast', 'MA' = 'Northeast', 'NH' = 'Northeast', 
  'RI' = 'Northeast', 'VT' = 'Northeast', 'NJ' = 'Northeast', 'NY' = 'Northeast', 
  'PA' = 'Northeast', 'IL' = 'Midwest', 'IN' = 'Midwest', 'MI' = 'Midwest', 
  'OH' = 'Midwest', 'WI' = 'Midwest', 'IA' = 'Midwest', 'KS' = 'Midwest', 
  'MN' = 'Midwest', 'MO' = 'Midwest', 'NE' = 'Midwest', 'ND' = 'Midwest', 
  'SD' = 'Midwest', 'DE' = 'South', 'FL' = 'South', 'GA' = 'South', 'MD' = 'South', 
  'NC' = 'South', 'SC' = 'South', 'VA' = 'South', 'WV' = 'South', 'AL' = 'South', 
  'KY' = 'South', 'MS' = 'South', 'TN' = 'South', 'AR' = 'South', 'LA' = 'South', 
  'OK' = 'South', 'TX' = 'South', 'DC' = 'South', 'AZ' = 'West', 'CO' = 'West', 
  'ID' = 'West', 'MT' = 'West', 'NV' = 'West', 'NM' = 'West', 'UT' = 'West', 
  'WY' = 'West', 'AK' = 'West', 'CA' = 'West', 'HI' = 'West', 'OR' = 'West', 
  'WA' = 'West'
)

# Add the Region column based on the state mapping
all_cei_data$Region <- state_region_mapping[all_cei_data$State]

# Clean the data to remove rows where Region is missing (NA)
all_cei_data_clean <- all_cei_data %>%
  filter(!is.na(Region)) %>%
  group_by(Year) %>%
  summarise(mean_cei_rating = mean(`CEI Rating`, na.rm = TRUE))

# Convert the aggregated data into a time series object
cei_ts <- ts(all_cei_data_clean$mean_cei_rating, start = min(all_cei_data_clean$Year), frequency = 1)

# Fit an ARIMA model to the national-level CEI data
fit_arima_national <- auto.arima(cei_ts)

# Forecast CEI Ratings for the next 7 years (2024-2030)
forecast_arima_national <- forecast(fit_arima_national, h = 7)

# Plot the forecast
autoplot(forecast_arima_national) +
  labs(title = "ARIMA Forecast of National CEI Ratings (2002-2030)", x = "Year", y = "Mean CEI Rating") +
  theme_linedraw()


tsdiag(fit_arima_national)

adf.test(cei_ts)
print(cei_ts)

