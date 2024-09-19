#FIXED EFFECT REGRESSION ANALYSIS

# Load necessary libraries
library(plm)
library(dplyr)
library(readr)
library(ggplot2)

# Load and Combine CEI Data
# Define a list of filenames (if not already combined)
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

# Function to read and label the files with year
read_and_label <- function(file) {
  year <- as.numeric(gsub("\\D", "", basename(file)))  # Extract year from filename
  data <- read_csv(file)
  data$Year <- year
  return(data)
}

# Read all files and combine them into one dataset
all_cei_data <- bind_rows(lapply(file_list, read_and_label))

# Prepare the Data
# Check the column names and fix any column with spaces or special characters
colnames(all_cei_data)  # Check the names of the columns

all_cei_data_clean <- all_cei_data %>%
  rename(CEI_Rating = `CEI Rating`) %>%
  select(City, Year, CEI_Rating)  # Select only relevant columns for analysis


selected_cities <- c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", 
                     "Seattle", "Miami", "Washington DC", "Boston", "Houston", 
                     "Denver", "San Diego", "Minneapolis", "Phoenix", "Philadelphia", 
                     "Orlando", "Dallas", "Austin", "New Orleans", "Portland")

# Filter the data to include only the 20 selected cities
all_cei_data_clean <- all_cei_data_clean %>%
  filter(City %in% selected_cities)

# Verify the filtered dataset
unique(all_cei_data_clean$City)  # This should return only the 20 selected cities

# Convert to a Panel Data Structure for Fixed Effects Model
panel_data <- pdata.frame(all_cei_data_clean, index = c("City", "Year"))

# Fit the Fixed Effects Model
# Regress CEI_Rating on Year
fixed_effects_model <- plm(CEI_Rating ~ Year, data = panel_data, model = "within")

# View the model summary
summary(fixed_effects_model)

# Visualization of Time Trend for CEI Scores Across Cities
ggplot(all_cei_data_clean, aes(x = Year, y = CEI_Rating, color = City)) +
  geom_line() +
  labs(title = "CEI Rating Trends Across 20 Selected Cities (2002-2023)", x = "Year", y = "CEI Rating") +
  theme_minimal()


# Heatmap of CEI Ratings
ggplot(all_cei_data_clean, aes(x = as.factor(Year), y = City, fill = CEI_Rating)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Heatmap of CEI Ratings Across Cities (2002-2023)", x = "Year", y = "City", fill = "CEI Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotates x-axis labels for better readability

# LOESS smoothing for CEI Rating trend across cities
ggplot(all_cei_data_clean, aes(x = Year, y = CEI_Rating, color = City)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Smoothed CEI Rating Trends Across Cities (2002-2023)", x = "Year", y = "CEI Rating") +
  theme_minimal()

# Facet plot by City
ggplot(all_cei_data_clean, aes(x = Year, y = CEI_Rating)) +
  geom_line() +
  facet_wrap(~ City, scales = "free_y", ncol = 4) +  # Creates a 4-column grid of individual plots for each city
  labs(title = "CEI Rating Trends Across 20 Selected Cities (2002-2023)", x = "Year", y = "CEI Rating") +
  theme_minimal()

# Fitted values (predicted CEI ratings) from the Fixed Effects model
all_cei_data_clean$predicted_CEI_Rating <- fitted(fixed_effects_model)

# Plot for the actual vs predicted CEI ratings over time for each city
ggplot(all_cei_data_clean, aes(x = Year, y = CEI_Rating, color = City)) +
  geom_line(aes(linetype = "Actual"), size = 1) +  # Actual CEI ratings
  geom_line(aes(y = predicted_CEI_Rating, linetype = "Predicted"), size = 1) +  # Predicted CEI ratings
  labs(title = "Actual vs Predicted CEI Ratings Across Cities", x = "Year", y = "CEI Rating") +
  theme_minimal() +
  scale_linetype_manual(name = "Legend", values = c("Actual" = "solid", "Predicted" = "dashed"))

# Extract fixed effects (city-specific effects)
city_fixed_effects <- fixef(fixed_effects_model)

# Convert to a dataframe for plotting
city_fixed_effects_df <- data.frame(City = names(city_fixed_effects), Fixed_Effect = as.numeric(city_fixed_effects))

# Plot the fixed effects for each city
ggplot(city_fixed_effects_df, aes(x = reorder(City, Fixed_Effect), y = Fixed_Effect, fill = City)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Fixed Effects for Each City (Controlling for Year)", x = "City", y = "Fixed Effect (Alpha)") +
  theme_minimal()

# Use a loess smoother to show the general trend across all cities
ggplot(all_cei_data_clean, aes(x = Year, y = CEI_Rating)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Smoothed CEI Rating Trends Over Time", x = "Year", y = "CEI Rating") +
  theme_minimal()


summary(fixed_effects_model)

# Extract fixed effects (city-specific intercepts)
city_fixed_effects <- fixef(fixed_effects_model)

# Convert to dataframe for plotting
city_fixed_effects_df <- data.frame(City = names(city_fixed_effects), Fixed_Effect = as.numeric(city_fixed_effects))

# Plot the fixed effects for each city
ggplot(city_fixed_effects_df, aes(x = reorder(City, Fixed_Effect), y = Fixed_Effect, fill = City)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip axes for readability
  labs(title = "City-Specific Fixed Effects for CEI Ratings (Controlling for Year)", x = "City", y = "Fixed Effect") +
  theme_minimal()

# Add predicted values from the Fixed Effects model to the dataset
all_cei_data_clean$predicted_CEI_Rating <- fitted(fixed_effects_model)

# Plot actual vs predicted CEI Ratings over time
ggplot(all_cei_data_clean, aes(x = Year, y = CEI_Rating, color = City)) +
  geom_line(aes(linetype = "Actual"), size = 1) +  # Actual CEI ratings
  geom_line(aes(y = predicted_CEI_Rating, linetype = "Predicted"), size = 1, color = "black") +  # Predicted CEI ratings
  labs(title = "Actual vs Predicted CEI Ratings Across Cities (Fixed Effects Model)", x = "Year", y = "CEI Rating") +
  theme_minimal() +
  scale_linetype_manual(name = "Legend", values = c("Actual" = "solid", "Predicted" = "dashed"))