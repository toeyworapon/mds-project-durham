# SPONSORSHIP DATA EXPLORATORY ANALYSIS

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Load the Sponsorship data
sponsorship_data <- read.csv("Pride_Sponsorship_Data.csv")

# Preview the data
head(sponsorship_data)

# Clean Sponsorship Data - Ensure 'City' and 'Employer' columns are properly formatted
sponsorship_data <- sponsorship_data %>%
  mutate(City = trimws(City),  # Remove extra spaces
         Employer = trimws(Employer))  # Remove extra spaces

# Check for missing values
sum(is.na(sponsorship_data))

# Combine Sponsorship Data with CEI Data
merged_sponsorship_cei <- sponsorship_data %>%
  left_join(cei_data, by = "Employer")  # Joining by 'Employer'

# Preview the merged data
head(merged_sponsorship_cei)

# Check for missing values after merging (to see if any companies didn't have CEI ratings)
sum(is.na(merged_sponsorship_cei$CEI))


# Calculate the average CEI Rating by City
city_cei_avg <- merged_sponsorship_cei %>%
  filter(!is.na(CEI)) %>%  # Exclude companies without CEI ratings
  group_by(City.x) %>%
  summarise(Average_CEI_Rating = mean(CEI, na.rm = TRUE),
            Num_Sponsors = n())  # Count number of sponsors per city

# Preview the city-level CEI average
print(city_cei_avg)


# Plot the density map
ggplot(merged_data, aes(x = `CEI Rating`, fill = City)) + 
  geom_density(alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Density Plot of CEI Ratings", x = "CEI Rating", y = "Density")



library(ggridges)

ggplot(merged_data, aes(x = `CEI Rating`, y = City, fill = City)) + 
  geom_density_ridges() + 
  theme_minimal() + 
  labs(title = "Ridgeline Plot of CEI Ratings by City", x = "CEI Rating", y = "City")

ggplot(merged_data, aes(x = `CEI Rating`, fill = City)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~City) + 
  theme_minimal() + 
  labs(title = "Distribution of CEI Ratings by City", x = "CEI Rating", y = "Frequency")


