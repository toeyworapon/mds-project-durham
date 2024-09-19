# CORRELATION ANALYSIS FOR LEGISLATIVE DATA

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)

# Step 1: Load CEI data files
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

# Function to read and clean CEI data with state information
read_and_clean_cei <- function(file) {
  data <- read_csv(file)
  year <- as.numeric(gsub("\\D", "", basename(file)))  # Extract year from filename
  data$Year <- year
  return(data %>% select(Employer, City, State, `CEI Rating`, Year))  # Ensure to include City and State
}

# Step 2: Combine all CEI data into a single dataframe
cei_data_new <- bind_rows(lapply(file_list, read_and_clean_cei))

# Step 3: Calculate average CEI Rating by State
avg_cei_data_new <- cei_data_new %>%
  group_by(State) %>%
  summarise(Average_CEI = mean(`CEI Rating`, na.rm = TRUE))

# Step 4: Load anti-LGBTQ legislative data
anti_lgbtq_data_new <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Step 5: Create total number of bills per state
# Summarize the data by state to count total anti-LGBTQ bills
total_bills_per_state <- anti_lgbtq_data_new %>%
  group_by(State) %>%
  summarise(Total_Bills = n())

# Step 6: Merge the average CEI rating with total anti-LGBTQ bills
merged_data_new <- left_join(avg_cei_data_new, total_bills_per_state, by = "State")

# Check the merged data
head(merged_data_new)

# Step 7: Correlation analysis and visualization
# Calculate the correlation between average CEI Rating and Total number of anti-LGBTQ bills
correlation_result <- cor(merged_data_new$Average_CEI, merged_data_new$Total_Bills, use = "complete.obs")

# Print correlation result
print(correlation_result)

# Plot the relationship between CEI and anti-LGBTQ bills
ggplot(merged_data_new, aes(x = Total_Bills, y = Average_CEI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship between Total Anti-LGBTQ Bills and Average CEI Rating in Selected States",
       x = "Total Anti-LGBTQ Bills",
       y = "Average CEI Rating") +
  theme_minimal()

# Save the plot
ggsave("anti_lgbtq_bills_vs_cei_rating_selected_states.svg", width = 12, height = 8)


# Scatter plot with state labels
ggplot(merged_data_new, aes(x = Total_Bills, y = Average_CEI, label = State)) +
  geom_point() +
  geom_text(vjust = -1, hjust = 1) +  # Add state labels
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship between Total Anti-LGBTQ Bills and Average CEI Rating by State",
       x = "Total Anti-LGBTQ Bills",
       y = "Average CEI Rating") +
  theme_linedraw()

# Save the plot
ggsave("anti_lgbtq_bills_vs_cei_rating_with_labels.svg", width = 12, height = 8)






# Install ggrepel package if you haven't already
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

# Scatter plot with state labels using ggrepel to prevent overlaps
ggplot(merged_data_new, aes(x = Total_Bills, y = Average_CEI, label = State)) +
  geom_point() +
  geom_text_repel(size = 3.5) +  # Use geom_text_repel to adjust label positions and prevent overlap
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship between Total Anti-LGBTQ Bills and Average CEI Rating by State",
       x = "Total Anti-LGBTQ Bills",
       y = "Average CEI Rating") +
  theme_linedraw()

# Save the updated plot
ggsave("anti_lgbtq_bills_vs_cei_rating_with_repel_labels.svg", width = 12, height = 8)

