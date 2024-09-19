# Install required packages
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("ggthemes")
install.packages("viridis")

# Load necessary libraries
library(ggplot2)
library(maps)
library(mapdata)
library(ggthemes)  # For color-blind friendly palettes
library(viridis)   # For color-blind friendly colors

# Get US Map Data
us_states <- map_data("state")

# Define States and Cities to Highlight
highlight_states <- c("new york", "california", "illinois", "georgia", "washington",
                      "florida", "massachusetts", "texas", "colorado", "minnesota",
                      "arizona", "pennsylvania", "louisiana", "oregon")

# Add highlight variable to states data
us_states$highlight <- ifelse(us_states$region %in% highlight_states, "Highlighted", "Normal")

# 3. Create a Data Frame with City Coordinates
city_coords <- data.frame(
  city = c("New York", "San Francisco", "Los Angeles", "Chicago", "Atlanta",
           "Seattle", "Miami", "Washington D.C.", "Boston", "Houston", "Denver",
           "San Diego", "Minneapolis", "Phoenix", "Philadelphia", "Orlando",
           "Dallas", "Austin", "New Orleans", "Portland"),
  state = c("NY", "CA", "CA", "IL", "GA", "WA", "FL", "DC", "MA", "TX", "CO",
            "CA", "MN", "AZ", "PA", "FL", "TX", "TX", "LA", "OR"),
  lat = c(40.7128, 37.7749, 34.0522, 41.8781, 33.7490, 47.6062, 25.7617,
          38.9072, 42.3601, 29.7604, 39.7392, 32.7157, 44.9778, 33.4484,
          39.9526, 28.5383, 32.7767, 30.2672, 29.9511, 45.5152),
  long = c(-74.0060, -122.4194, -118.2437, -87.6298, -84.3880, -122.3321,
           -80.1918, -77.0369, -71.0589, -95.3698, -104.9903, -117.1611,
           -93.2650, -112.0740, -75.1652, -81.3792, -96.7970, -97.7431,
           -90.0715, -122.6784),
  stringsAsFactors = FALSE
)

# 4. Plot the Map with Highlighted States and Cities
ggplot() +
  # Draw the states with appropriate fill colors
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group, fill = highlight),
               color = "white", size = 0.2) +
  # Add the cities as points
  geom_point(data = city_coords, aes(x = long, y = lat), color = "#D55E00", size = 2) +
  # Define fill colors using a color-blind friendly palette
  scale_fill_manual(values = c("Highlighted" = "#0072B2", "Normal" = "grey80")) +
  # Fix the coordinate ratio
  coord_fixed(1.3) +
  # Simplify the plot theme
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("US Map for Selected Cities in this study")
