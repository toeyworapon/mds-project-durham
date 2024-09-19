# TIME-SERIES ANALYSIS FOR ARIMA

# Install and load necessary packages
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
library(patchwork)  # For combining plots

# Define the list of cities to analyze
cities <- c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", "Seattle", 
            "Miami", "Washington DC", "Boston", "Houston", "Denver", "San Diego", 
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
plot_list <- list()  # List to store plots

# Loop through each city
for (city in cities) {
  
  # Initialize a dataframe to accumulate data for this city across years
  city_data_list <- list()
  
  # Loop through each file and extract city-specific data for each year
  for (file in file_list) {
    
    # Extract the year from the file names
    year <- as.numeric(gsub("\\D", "", basename(file)))
    
    # Read the CSV file
    temp_data <- read_csv(file)
    
    # Filter for the current city
    city_data_year <- temp_data %>%
      filter(trimws(City) == city) %>%  
      mutate(Year = year)
    
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
      labs(title = paste("ARIMA's CEI Forecast for", city), 
           x = "Year", y = "Mean CEI Rating") +
      theme_minimal()
    
    # Store the plot in the plot_list
    plot_list[[city]] <- city_forecast_plot
    
  } else {
    print(paste("Not enough data for ARIMA analysis in", city))
  }
}

# Combine all the plots into one figure using patchwork
combined_plot <- wrap_plots(plot_list, ncol = 4)

# Print the combined plot
print(combined_plot)

svg("ARIMA_Forecast_HighRes.svg", width = 7, height = 5)  # Vector SVG
autoplot(forecast_arima_national) + 
  labs(title = "ARIMA Forecast of National CEI Ratings", x = "Year", y = "CEI Rating") + 
  theme_minimal()
dev.off()  # Close the SVG device


# RESIDUALS ANALYSIS

# Install and load necessary packages
install.packages("patchwork")
library(patchwork)
library(ggplot2)

# Define the function to generate residual plot and Q-Q plot for ARIMA models and return both plots
generate_residual_qq_plots <- function(arima_model, city) {
  
  # Residual plot
  residual_plot <- autoplot(arima_model$residuals) + 
    ggtitle(paste("Residuals for", city)) +
    theme_minimal()
  
  # Normal Q-Q plot
  qq_plot <- ggplot(data = data.frame(resid = arima_model$residuals), aes(sample = resid)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Q-Q Plot for", city)) +
    theme_minimal()
  
  return(list(residual_plot = residual_plot, qq_plot = qq_plot))  # Return both plots in a list
}

# Define the list of cities to analyze
cities <- c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", "Seattle", 
            "Miami", "Washington DC", "Boston", "Houston", "Denver", "San Diego", 
            "Minneapolis", "Phoenix", "Philadelphia", "Orlando", "Dallas", "Austin", 
            "New Orleans", "Portland")

# Lists to store all the residual and Q-Q plots separately
residual_plots <- list()
qq_plots <- list()

# Loop through each city and generate the diagnostic plots
for (city in cities) {
  if (!is.null(city_arima_results[[city]]$model)) {  # Check if model exists for the city
    plots <- generate_residual_qq_plots(city_arima_results[[city]]$model, city)
    residual_plots[[city]] <- plots$residual_plot  # Store the residual plot
    qq_plots[[city]] <- plots$qq_plot              # Store the Q-Q plot
  } else {
    print(paste("No ARIMA model found for", city))
  }
}

# Combine all residual plots in a 4-column grid layout
combined_residual_plots <- wrap_plots(residual_plots, ncol = 4)

# Combine all Q-Q plots in a 4-column grid layout
combined_qq_plots <- wrap_plots(qq_plots, ncol = 4)

# Display the combined residual plots
print(combined_residual_plots)

# Display the combined Q-Q plots
print(combined_qq_plots)


# TIME SERIES DIAGNOSIS

# Define the list of cities to analyze
cities <- c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", "Seattle", 
            "Miami", "Washington DC", "Boston", "Houston", "Denver", "San Diego", 
            "Minneapolis", "Phoenix", "Philadelphia", "Orlando", "Dallas", "Austin", 
            "New Orleans", "Portland")

# Loop through each city and run tsdiag for diagnostics
for (city in cities) {
  if (!is.null(city_arima_results[[city]]$model)) {  # Check if model exists for the city
    print(paste("Running tsdiag for", city))
    
    # Run tsdiag for the ARIMA model and plot the results
    tsdiag(city_arima_results[[city]]$model)
    
  } else {
    print(paste("No ARIMA model found for", city))
  }
}

# LJUNG-BOX TEST

# Create a function to extract Ljung-Box test results with improved formatting
get_ljung_box_results_clean <- function(city_name, model) {
  test_result <- Box.test(model$residuals, type = "Ljung-Box")
  data.frame(
    City = city_name,
    Ljung_Box_Statistic = round(test_result$statistic, 2),  # Rounded for clarity
    df = test_result$parameter,
    p_value = round(test_result$p.value, 4)  # Rounded to 4 decimal places
  )
}

# Initialize an empty data frame to store the results
ljung_box_results_clean <- data.frame()

# Loop through each city's ARIMA model and store the test results
for (city in cities) {
  if (!is.null(city_arima_results[[city]]$model)) {
    result <- get_ljung_box_results_clean(city, city_arima_results[[city]]$model)
    ljung_box_results_clean <- rbind(ljung_box_results_clean, result)
  }
}

# Display the cleaned results
print(ljung_box_results_clean)


# ADF TEST

# Load the required library
library(tseries)

# Function to run ADF test for a city's time series
run_adf_test <- function(city_name, city_data) {
  print(paste("ADF Test for", city_name))
  adf_result <- adf.test(city_data$`CEI Rating`, alternative = "stationary")
  print(adf_result)
}

# Run ADF test for each city
for (city in cities) {
  city_data <- all_cei_data %>% filter(City == city)
  run_adf_test(city, city_data)
}



