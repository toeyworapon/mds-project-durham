# CEI Cities Analysis

# Install and load necessary libraries
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("readr")) install.packages("readr")

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Define the list of cities to analyze (the 20 cities you are interested in)
selected_cities <- c(
  "New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", "Seattle", 
  "Miami", "Washington DC", "Boston", "Houston", "Denver", "San Diego", 
  "Minneapolis", "Phoenix", "Philadelphia", "Orlando", "Dallas", "Austin", 
  "New Orleans", "Portland"
)

# Load files
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

# Function to read and prepare data for each year
read_and_prepare_cei <- function(file) {
  # Extract year from the filename
  year <- as.numeric(gsub("\\D", "", basename(file)))
  
  # Read the CSV file
  data <- read_csv(file)
  
  # Add a 'Year' column
  data <- data %>% mutate(Year = year)
  
  # Select only necessary columns ('City', 'Employer', 'CEI Rating', 'Year')
  data %>% select(City, `CEI Rating`, Year)
}

# Combine all data into one dataframe
cei_data <- bind_rows(lapply(file_list, read_and_prepare_cei))

# Filter data to include only the selected cities and exclude 2007
cei_data_filtered <- cei_data %>% 
  filter(City %in% selected_cities & Year != 2007)

# Prepare data for heatmap
# Group by City and Year to get the average CEI Rating per city per year
heatmap_data <- cei_data_filtered %>%
  group_by(City, Year) %>%
  summarise(Avg_CEI_Rating = mean(`CEI Rating`, na.rm = TRUE)) %>%
  ungroup()

# Check the processed heatmap data
head(heatmap_data)

# Plot the heatmap using ggplot2
ggplot(heatmap_data, aes(x = Year, y = City, fill = Avg_CEI_Rating)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(na.value = "white", name = "CEI Rating") +
  scale_x_continuous(breaks = seq(min(heatmap_data$Year), max(heatmap_data$Year), by = 1)) +  # Ensure all years are shown except 2007
  labs(title = "Heatmap of CEI Ratings Across 20 Selected Cities (2002-2023)", x = "Year", y = "City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggsave("us_heatmap_average_labels.svg", width = 10, height = 8)
