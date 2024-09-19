# ARIMA STATES FORECAST ANALYSIS

# Install and load necessary packages if not already installed
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("patchwork")


# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)


# Define the list of cities to analyze
cities <- c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", "Seattle", 
            "Miami", "Washington D.C.", "Boston", "Houston", "Denver", "San Diego", 
            "Minneapolis", "Phoenix", "Philadelphia", "Orlando", "Dallas", "Austin", 
            "New Orleans", "Portland")

# Define a vector containing the filenames for each year, excluding 2007
file_list <- c(
  "CEI2002_ByCompany_Scores_REV.csv", "CEI2003_ByCompany_Scores_REV.csv", 
  "CEI2004_ByCompany_Scores_REV.csv", "CEI2005_ByCompany_Scores_REV.csv", 
  "CEI2006_ByCompany_Scores_REV.csv", "CEI2008_ByCompany_Scores_REV.csv", 
  "CEI2009_ByCompany_Scores_REV.csv", "CEI2010_ByCompany_Scores_REV.csv", 
  "CEI2011_ByCompany_Scores_REV.csv", "CEI2012_ByCompany_Scores_REV.csv", 
  "CEI2013_ByCompany_Scores_REV.csv", "CEI2014_ByCompany_Scores_REV.csv", 
  "CEI2015_ByCompany_Scores_REV.csv", "CEI2016_ByCompany_Scores_REV.csv", 
  "CEI2017_ByCompany_Scores_REV.csv", "CEI2018_ByCompany_Scores_REV.csv", 
  "CEI2019_ByCompany_Scores_REV.csv", "CEI2020_ByCompany_Scores_REV.csv", 
  "CEI2021_ByCompany_Scores_REV.csv", "CEI2022_ByCompany_Scores_REV.csv", 
  "CEI2023_ByCompany_Scores_REV.csv"
)

# Initialize a list to store results for all cities
city_arima_results <- list()

# Loop through each city
for (city in cities) {
  
  # Initialize a dataframe to accumulate data for this city across years
  city_data_list <- list()
  
  # Loop through each file and extract city-specific data for each year
  for (file in file_list) {
    
    # Extract the year from the filename (assuming 'CEI2002', 'CEI2003', etc.)
    year <- as.numeric(gsub("\\D", "", basename(file)))
    
    # Read the CSV file
    temp_data <- read_csv(file)
    
    # Filter for the current city
    city_data_year <- temp_data %>%
      filter(trimws(City) == city) %>%  # Ensure correct column name 'City'
      mutate(Year = year)               # Add the Year column
    
    # Append to the city data list
    city_data_list[[length(city_data_list) + 1]] <- city_data_year
  }
  
  # Combine all the yearly data for the current city into one dataframe
  city_data <- bind_rows(city_data_list)
  
  # Group by year and calculate the mean CEI score for this city
  city_data_grouped <- city_data %>%
    group_by(Year) %>%
    summarise(mean_cei_rating = mean(`CEI Rating`, na.rm = TRUE))
  
  # Check if there is enough data for ARIMA (at least 10 years)
  if (nrow(city_data_grouped) > 10) {
    
    # Convert to time series object
    city_ts <- ts(city_data_grouped$mean_cei_rating, start = min(city_data_grouped$Year), frequency = 1)
    
    # Check stationarity using ADF test
    adf_result <- adf.test(city_ts)
    print(paste("ADF test result for", city, ":"))
    print(adf_result)
    
    # Fit ARIMA model
    fit_arima_city <- auto.arima(city_ts)
    
    # Forecast the next 5 years
    forecast_city <- forecast(fit_arima_city, h = 5)
    
    # Save the ARIMA model and forecast in the list
    city_arima_results[[city]] <- list(
      model = fit_arima_city,
      forecast = forecast_city
    )
    
    # Plot the forecast for the city
    city_forecast_plot <- autoplot(forecast_city) +
      labs(title = paste("ARIMA Forecast for", city, "(CEI Ratings)"), 
           x = "Year", y = "Mean CEI Rating") +
      theme_minimal()
    
    # Print the plot to display it
    print(city_forecast_plot)
    
    # Save the plot as a file
    ggsave(paste0("ARIMA_Forecast_", gsub(" ", "_", city), ".png"), plot = city_forecast_plot) 
    
  } else {
    print(paste("Not enough data for ARIMA analysis in", city))
  }
}
