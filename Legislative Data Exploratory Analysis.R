# LEGISLATIVE DATA EXPLORATORY ANALYSIS

# Load the legislation dataset
legislation_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# View the first few rows of the data
head(legislation_data)

# Summary of the data
summary(legislation_data)

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
anti_lgbtq_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Summarize the data by counting the occurrences of each status for each state
status_summary <- anti_lgbtq_data %>%
  group_by(State) %>%
  summarise(
    Passed_Law = sum(Status == "Passed into Law", na.rm = TRUE),
    Advancing = sum(Status == "Advancing", na.rm = TRUE),
    Defeated = sum(Status == "Defeated", na.rm = TRUE)
  )

# Reshape the data for easier plotting
status_long <- tidyr::pivot_longer(status_summary, cols = c(Passed_Law, Advancing, Defeated), 
                                   names_to = "Status", values_to = "Count")

# Plot the data
ggplot(status_long, aes(x = reorder(State, -Count), y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Anti-LGBTQ Legislation by State", 
       x = "State", y = "Number of Bills", fill = "Bill Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Prepare the data for plotting heatmap
heatmap_data <- anti_lgbtq_data %>%
  group_by(State) %>%
  summarise(
    Passed_Law = sum(Status == "Passed into Law", na.rm = TRUE),
    Advancing = sum(Status == "Advancing", na.rm = TRUE),
    Defeated = sum(Status == "Defeated", na.rm = TRUE),
    Total = Passed_Law + Advancing + Defeated
  )

# Plot heatmap
ggplot(heatmap_data, aes(x = reorder(State, -Total), y = 1, fill = Total)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Heatmap of Anti-LGBTQ Legislation Activity by State",
       x = "State", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the ACLU data
aclu_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Split the 'Issues' column based on the "/" separator
aclu_data_split <- aclu_data %>%
  separate_rows(Issues, sep = "/")  # Separate each issue into its own row

# Summarize the data by State and Issue
state_issue_summary <- aclu_data_split %>%
  group_by(State, Issues) %>%
  summarise(Number_of_Bills = n()) %>%
  ungroup()

# Check for missing issues
missing_issues <- aclu_data_split %>%
  filter(is.na(Issues)) %>%
  distinct(Issues)

if (nrow(missing_issues) > 0) {
  print("Missing Issues not categorized:")
  print(missing_issues)
}

# Visualize the data
library(ggplot2)

ggplot(state_issue_summary, aes(x = reorder(State, -Number_of_Bills), y = Number_of_Bills, fill = Issues)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Count of Anti-LGBTQ Bills by State and Issue", x = "State", y = "Number of Bills", fill = "Issues") +
  theme_minimal()
ggsave("prelimcheck_legal_count.svg", width = 10, height = 8)


# ANALYSIS OF SELECTED 20 CITIES WITH PRELIMINARY CHECK

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the ACLU data
aclu_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Define the states corresponding to the 20 selected cities
selected_states <- c("CA", "TX", "NY", "IL", "PA", "DC", "GA", "WA", "MA", 
                     "FL", "MN", "CO", "LA", "AZ", "OR", "OH")


# Split the 'Issues' column based on the "/" separator
aclu_data_split <- aclu_data %>%
  separate_rows(Issues, sep = "/")  # Separate each issue into its own row

# Filter the data to only include the selected states
aclu_data_filtered <- aclu_data_split %>%
  filter(State %in% selected_states)

# Summarize the data by State and Issue
state_issue_summary <- aclu_data_filtered %>%
  group_by(State, Issues) %>%
  summarise(Number_of_Bills = n()) %>%
  ungroup()

# Check for missing issues (if any)
missing_issues <- aclu_data_split %>%
  filter(is.na(Issues)) %>%
  distinct(Issues)

if (nrow(missing_issues) > 0) {
  print("Missing Issues not categorized:")
  print(missing_issues)
}

# Visualize the data
ggplot(state_issue_summary, aes(x = reorder(State, -Number_of_Bills), y = Number_of_Bills, fill = Issues)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Count of Anti-LGBTQ Bills by State and Issue", x = "State", y = "Number of Bills", fill = "Issues") +
  theme_minimal()

# Save the plot in SVG
ggsave("prelimcheck_legal_count_20cities.svg", width = 10, height = 8)


# DATA COUNT

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the ACLU data 
aclu_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Define the manual mapping of state abbreviations to full names
aclu_data$State <- case_when(
  aclu_data$State == "CA" ~ "California",
  aclu_data$State == "TX" ~ "Texas",
  aclu_data$State == "NY" ~ "New York",
  aclu_data$State == "IL" ~ "Illinois",
  aclu_data$State == "PA" ~ "Pennsylvania",
  aclu_data$State == "DC" ~ "District of Columbia",
  aclu_data$State == "GA" ~ "Georgia",
  aclu_data$State == "WA" ~ "Washington",
  aclu_data$State == "MA" ~ "Massachusetts",
  aclu_data$State == "FL" ~ "Florida",
  aclu_data$State == "MN" ~ "Minnesota",
  aclu_data$State == "CO" ~ "Colorado",
  aclu_data$State == "LA" ~ "Louisiana",
  aclu_data$State == "AZ" ~ "Arizona",
  aclu_data$State == "OR" ~ "Oregon",
  aclu_data$State == "OH" ~ "Ohio",
  TRUE ~ aclu_data$State # Keep any other state names as they are
)

# Define the states corresponding to the 20 selected cities
selected_states <- c("California", "Texas", "New York", "Illinois", 
                     "Pennsylvania", "District of Columbia", "Georgia", 
                     "Washington", "Massachusetts", "Florida", 
                     "Minnesota", "Colorado", "Louisiana", "Arizona", 
                     "Oregon", "Ohio")

# Split the 'Issues' column based on the "/" separator
aclu_data_split <- aclu_data %>%
  separate_rows(Issues, sep = "/")  # Separate each issue into its own row

# Filter the data to only include the selected states
aclu_data_filtered <- aclu_data_split %>%
  filter(State %in% selected_states)

# Summarize the data by State and Issue
state_issue_summary <- aclu_data_filtered %>%
  group_by(State, Issues) %>%
  summarise(Number_of_Bills = n()) %>%
  ungroup()

# Check for missing issues
missing_issues <- aclu_data_split %>%
  filter(is.na(Issues)) %>%
  distinct(Issues)

if (nrow(missing_issues) > 0) {
  print("Missing Issues not categorized:")
  print(missing_issues)
}

# Visualize the data
ggplot(state_issue_summary, aes(x = reorder(State, -Number_of_Bills), y = Number_of_Bills, fill = Issues)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Count of Anti-LGBTQ Bills by State and Issue", 
       x = "State", y = "Number of Bills", fill = "Issues") +
  theme_minimal()

# Save the plot
ggsave("prelimcheck_legal_count_20states.svg", width = 10, height = 8)



# ISSUES-BASED DATA

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Load the ACLU data
aclu_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Define the manual mapping of state abbreviations to full names
aclu_data$State <- case_when(
  aclu_data$State == "CA" ~ "California",
  aclu_data$State == "TX" ~ "Texas",
  aclu_data$State == "NY" ~ "New York",
  aclu_data$State == "IL" ~ "Illinois",
  aclu_data$State == "PA" ~ "Pennsylvania",
  aclu_data$State == "DC" ~ "District of Columbia",
  aclu_data$State == "GA" ~ "Georgia",
  aclu_data$State == "WA" ~ "Washington",
  aclu_data$State == "MA" ~ "Massachusetts",
  aclu_data$State == "FL" ~ "Florida",
  aclu_data$State == "MN" ~ "Minnesota",
  aclu_data$State == "CO" ~ "Colorado",
  aclu_data$State == "LA" ~ "Louisiana",
  aclu_data$State == "AZ" ~ "Arizona",
  aclu_data$State == "OR" ~ "Oregon",
  aclu_data$State == "OH" ~ "Ohio",
  TRUE ~ aclu_data$State # Keep any other state names as they are
)

# Define the states corresponding to the 20 selected cities
selected_states <- c("California", "Texas", "New York", "Illinois", 
                     "Pennsylvania", "District of Columbia", "Georgia", 
                     "Washington", "Massachusetts", "Florida", 
                     "Minnesota", "Colorado", "Louisiana", "Arizona", 
                     "Oregon", "Ohio")

# Group similar issues into broader categories
aclu_data_split <- aclu_data %>%
  separate_rows(Issues, sep = "/") %>%
  mutate(
    Issue_Group = case_when(
      Issues %in% c("Healthcare age restrictions", "Healthcare funding restrictions", 
                    "Healthcare Restrictions", "Prison healthcare restrictions", 
                    "Other healthcare barriers") ~ "Healthcare-related",
      Issues %in% c("Curriculum censorship", "Forced outing in schools", 
                    "School facilities bans", "School sports bans", 
                    "Other school restrictions") ~ "School-related",
      Issues %in% c("Free Speech & Expression Bans", "Other expression restrictions") ~ "Free Speech-related",
      Issues %in% c("Religious exemptions", "Re-definition of sex", 
                    "Other civil rights restrictions") ~ "Civil Rights-related",
      Issues %in% c("Public Accommodation Bans", "Drag bans", "Barriers to Accurate IDs") ~ "Public & ID-related",
      TRUE ~ "Other Issues"
    )
  )

# Filter the data to only include the selected states
aclu_data_filtered <- aclu_data_split %>%
  filter(State %in% selected_states)

# Summarize the data by State and Issue_Group
state_issue_summary <- aclu_data_filtered %>%
  group_by(State, Issue_Group) %>%
  summarise(Number_of_Bills = n()) %>%
  ungroup()

# Visualize the data with a colorblind-friendly palette and text labels
ggplot(state_issue_summary, aes(x = reorder(State, -Number_of_Bills), y = Number_of_Bills, fill = Issue_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Number_of_Bills), position = position_stack(vjust = 0.5), size = 3) +  # Add text labels
  scale_fill_brewer(palette = "Set2") +  # Use a colorblind-friendly palette
  coord_flip() +
  labs(title = "Count of Anti-LGBTQ Bills by State and Issue Group", 
       x = "State", y = "Number of Bills", fill = "Issue Group") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move the legend to the bottom for better readability

# Save the plot
ggsave("prelimcheck_legal_count_20states_grouped.svg", width = 10, height = 8)



#ANALYSES

# Install and load necessary packages
if (!require(usmap)) install.packages("usmap")
if (!require(stringr)) install.packages("stringr")
library(usmap)
library(ggplot2)
library(dplyr)
library(stringr)

# Create a mapping of full state names to abbreviations
state_mapping <- data.frame(
  full_name = state.name,
  abbrev = state.abb,
  stringsAsFactors = FALSE
)

# Add DC to the mapping (as technically it's not a state)
state_mapping <- rbind(state_mapping, c("District of Columbia", "DC"))

# Calculate state totals
state_totals <- aclu_data %>%
  group_by(State) %>%
  summarise(Total_Bills = n()) %>%
  ungroup()

# Map full state names to abbreviations
state_totals <- state_totals %>%
  left_join(state_mapping, by = c("State" = "full_name")) %>%
  mutate(state = coalesce(abbrev, State)) %>%
  select(state, Total_Bills)

# Get centroid coordinates for each state
centroid_coords <- usmapdata::centroid_labels("state")

# Merge state totals with centroid coordinates
state_data <- left_join(centroid_coords, state_totals, by = "state")

# Create the map
plot_usmap(data = state_totals, values = "Total_Bills", color = "black") +
  geom_text(data = state_data, 
            aes(x = x, y = y, label = state),
            color = "black", size = 3) +
  scale_fill_continuous(
    low = "white", 
    high = "red", 
    name = "Number of Bills",
    label = scales::comma
  ) +
  labs(title = "Total Anti-LGBTQ Bills by State") +
  theme(legend.position = "right")

# Save the plot
ggsave("us_map_anti_lgbtq_bills_with_abbrev.svg", width = 12, height = 8)

# Print any states that didn't get mapped correctly
unmapped_states <- state_totals %>% 
  filter(is.na(state)) %>% 
  pull(State)

if (length(unmapped_states) > 0) {
  print("The following states couldn't be mapped:")
  print(unmapped_states)
}


# Load necessary libraries if not already loaded
library(dplyr)
library(ggplot2)

status_breakdown <- aclu_data %>%
  group_by(State, Status) %>%
  summarise(Count = n()) %>%
  ungroup()

ggplot(status_breakdown, aes(x = reorder(State, -Count), y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Detailed Breakdown of Anti-LGBTQ Legislation Status by State",
       x = "State", y = "Number of Bills", fill = "Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("legislation_status_breakdown.svg", width = 12, height = 10)
