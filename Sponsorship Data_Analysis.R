# SPONSORSHIP ANALYSIS

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Load sponsorship data
sponsorship_data <- read.csv("Pride_Sponsorship_Data.csv")

# List of CEI files
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

# Function to read each file, extract relevant columns, and add the year
read_and_combine_cei_files <- function(file) {
  year <- as.numeric(gsub("\\D", "", basename(file)))  # Extract year from the filename
  data <- read_csv(file)
  data$Year <- year  # Add the Year column
  return(data %>% select(`Employer`, `CEI Rating`, Year))  # Keep only relevant columns
}

# Combine all CEI data into one dataframe
combined_cei_data <- bind_rows(lapply(file_list, read_and_combine_cei_files))

# Merge the sponsorship data with the combined CEI data
# Ensure the column names match between datasets
colnames(sponsorship_data)[colnames(sponsorship_data) == "Sponsor Name"] <- "Employer"
merged_data <- left_join(sponsorship_data, combined_cei_data, by = "Employer", "Year")

# Check merged data
head(merged_data)


# Visualize average CEI rating by city
ggplot(city_cas, aes(x = reorder(City, Average_CEI_Rating), y = Average_CEI_Rating)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Average CEI Rating by City", x = "City", y = "Average CEI Rating") +
  theme_minimal()


# Create a sample dataset with 'Num_Sponsors' column representing number of sponsors for each city
city_cas$Num_Sponsors <- sample(1:50, size = nrow(city_cas), replace = TRUE)

# Bubble plot
ggplot(city_cas, aes(x = reorder(City, Average_CEI_Rating), y = Average_CEI_Rating, size = Num_Sponsors, color = Average_CEI_Rating)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 12)) +
  scale_color_gradient(low = "blue", high = "red") +
  coord_flip() +
  labs(title = "CEI Rating based on each city based on Pride Corporate Sponsorship", x = "City", y = "Average CEI Rating", size = "Number of Sponsors") +
  theme_linedraw()
ggsave("us_map_bubble_labels_linedraw.svg", width = 10, height = 8)


# Calculate the minimum, maximum, and average CEI ratings for each city
city_cei_summary <- merged_data %>%
  group_by(City) %>%
  summarise(
    Min_CEI_Rating = min(`CEI Rating`, na.rm = TRUE),
    Max_CEI_Rating = max(`CEI Rating`, na.rm = TRUE),
    Avg_CEI_Rating = mean(`CEI Rating`, na.rm = TRUE),
    Gap = Max_CEI_Rating - Min_CEI_Rating  # Calculate the gap
  ) %>%
  arrange(desc(Gap))  # Sort by the gap to see the cities with the largest difference

# Check the summary
print(city_cei_summary)


# Error bar plot showing CEI rating range (minimum to maximum) for each city
ggplot(city_cei_summary, aes(x = reorder(City, Avg_CEI_Rating), y = Avg_CEI_Rating)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = Min_CEI_Rating, ymax = Max_CEI_Rating), width = 0.2, color = "grey") +
  coord_flip() +
  labs(title = "CEI Rating Ranges by City (Error Bar Chart)", x = "City", y = "CEI Rating") +
  theme_minimal()

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)

# Data preparation
# City coordinates: Ensure you add the correct latitude and longitude for each city in your dataset
city_coordinates <- data.frame(
  City = c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta", "Seattle", 
           "Miami", "Washington DC", "Boston", "Houston", "Denver", "San Diego",
           "Minneapolis", "Phoenix", "Philadelphia", "Orlando", "Dallas", "Austin",
           "New Orleans", "Portland"),
  Latitude = c(40.7128, 37.7749, 34.0522, 41.8781, 33.7490, 47.6062,
               25.7617, 38.9072, 42.3601, 29.7604, 39.7392, 32.7157,
               44.9778, 33.4484, 39.9526, 28.5383, 32.7767, 30.2672,
               29.9511, 45.5155),
  Longitude = c(-74.0060, -122.4194, -118.2437, -87.6298, -84.3880, -122.3321,
                -80.1918, -77.0369, -71.0589, -95.3698, -104.9903, -117.1611,
                -93.2650, -112.0740, -75.1652, -81.3792, -96.7970, -97.7431,
                -90.0715, -122.6793)
)

# Add state information for cities
city_coordinates <- city_coordinates %>%
  mutate(State = c("New York", "California", "California", "Illinois", "Georgia", "Washington",
                   "Florida", "District of Columbia", "Massachusetts", "Texas", "Colorado", "California",
                   "Minnesota", "Arizona", "Pennsylvania", "Florida", "Texas", "Texas",
                   "Louisiana", "Oregon"))

# Example CEI rating and number of sponsors
city_cei_map <- city_coordinates %>%
  mutate(Average_CEI_Rating = sample(60:100, size = 20, replace = TRUE), # Example CEI data
         Num_Sponsors = sample(10:50, size = 20, replace = TRUE))        # Example sponsor data

# Load US map data
us_map <- map_data("state")

# Get unique states where the cities are located
highlighted_states <- unique(city_coordinates$State)

# Highlight the states containing cities by merging state data
state_data <- us_map %>%
  mutate(State = tolower(region)) %>%
  filter(State %in% tolower(highlighted_states))  # Keep only the states of interest

# Plot the US map with city points, city labels, and highlighted states
ggplot() +
  # Draw the full US map with light grey color
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey97", color = "white") +
  
  # Highlight the states that contain the cities
  geom_polygon(data = state_data, aes(x = long, y = lat, group = group), fill = "grey70", color = "white") +
  
  # Add city points
  geom_point(data = city_cei_map, aes(x = Longitude, y = Latitude, size = Num_Sponsors, color = Average_CEI_Rating), alpha = 0.8) +
  
  # Add city labels
  geom_text(data = city_cei_map, aes(x = Longitude, y = Latitude, label = City), vjust = -1, size = 3, color = "black") +
  
  # Color scale for CEI ratings
  scale_color_gradient(low = "blue", high = "red") +
  
  # Adjust point size
  scale_size(range = c(3, 10)) +
  
  # Labels for the plot
  labs(title = "Average CEI Ratings Across US Cities (Highlighted States)", 
       x = "", y = "", color = "Avg CEI Rating", size = "Num Sponsors") +
  
  # Theme settings
  theme_void() +
  theme(legend.position = "right")

# Boxplot of CEI Ratings by City
ggplot(merged_data, aes(x = reorder(City, `CEI Rating`), y = `CEI Rating`)) +
  geom_boxplot(aes(fill = City)) +
  coord_flip() +
  labs(title = "Distribution of CEI Ratings by City", 
       x = "City", y = "CEI Rating") +
  theme_linedraw()
ggsave("ceiratingsdistribute_city_2.svg", width = 10, height = 8)


# CORRELATION ANALYSIS ON CEI RATING and CITIES

# Correlation between Number of Sponsors and Average CEI Rating
cor(city_cei_avg$Num_Sponsors, city_cei_avg$Average_CEI_Rating)

# Scatter plot with regression line
ggplot(city_cei_avg, aes(x = Num_Sponsors, y = Average_CEI_Rating)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation Between Number of Sponsors and Average CEI Rating",
       x = "Number of Sponsors", y = "Average CEI Rating") +
  theme_linedraw()
ggsave("correlation_spon_avgratedCEI.svg", width = 10, height = 8)

# Visualize the authenticity score
ggplot(city_cei_authenticity_score, aes(x = reorder(City.x, Authenticity_Score), y = Authenticity_Score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Corporate Authenticity Score by City", 
       x = "City", y = "Authenticity Score") +
  theme_minimal()

# Count of CEI Rated vs Non-Rated Companies per City
cei_vs_non_cei <- merged_data %>%
  group_by(City, `CEI Rating`) %>%
  summarise(Num_Companies = n()) %>%
  spread(`CEI Rating`, Num_Companies, fill = 0)



