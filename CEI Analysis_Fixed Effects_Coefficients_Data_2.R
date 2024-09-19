# Load necessary libraries
library(plm)
library(dplyr)
library(ggplot2)
library(broom)

panel_data <- pdata.frame(panel_data, index = c("City", "Year"))

# Perform Fixed Effects Regression with interaction term for City
fixed_effects_city_model <- plm(CEI_Rating ~ Year + City, data = panel_data, model = "within")

summary(fixed_effects_city_model)

# Extract the coefficients for each city
coef_df <- tidy(fixed_effects_city_model)

# Extract interaction terms between Year and City
coef_df <- coef_df %>%
  mutate(City = gsub("Year|:", "", term)) %>%
  filter(grepl("Year", term))

ggplot(coef_df, aes(x = term, y = estimate, color = City)) +
  geom_point() +
  geom_line(aes(group = City)) + # Ensure the lines connect for each city
  labs(title = "Fixed Effects Coefficients for Year by City",
       x = "Year",
       y = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plot with horizontal bars and color gradient
ggplot(city_effects_df, aes(x = reorder(City, City_Effect), y = City_Effect, fill = City_Effect)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Cities' Fixed Effects Regression Model from CEI Ratings 2002-2023",
       x = "City",
       y = "Fixed Effect Estimate") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 0)) +
  coord_flip()  # This makes the bars horizontal
ggsave("coeff_bars.svg", width = 10, height = 8)


# Lollipop Chart for City Fixed Effects
ggplot(city_effects_df, aes(x = reorder(City, City_Effect), y = City_Effect)) +
  geom_segment(aes(xend = reorder(City, City_Effect), y = 0, yend = City_Effect), color = "grey") +
  geom_point(size = 4, color = "darkblue") +
  labs(title = "City Fixed Effects from CEI Ratings Model",
       x = "City",
       y = "Fixed Effect Estimate") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12, angle = 0)) +
  coord_flip()  # This flips the axes for easier reading

# Add confidence intervals to the bar plot
ggplot(coef_df, aes(x = reorder(City, estimate), y = estimate)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, color = "grey") +
  labs(title = "City Fixed Effects with Confidence Intervals", x = "City", y = "Fixed Effect Estimate") +
  theme_minimal() +
  coord_flip()
