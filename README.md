# mds-project-durham
The repository for the MDS Research Project, University of Durham for a student with anonymous marking number 


# Corporate Equality Index (CEI) & Anti-LGBTQ Legislation Analysis

This project analyzes Corporate Equality Index (CEI) ratings and anti-LGBTQ legislation across different U.S. regions. The analysis uses various statistical and time-series forecasting methods such as ARIMA and ETS models to understand trends, regional differences, and the relationship between CEI ratings and legislative activities.

## Table of Contents

- [Introduction](#introduction)
- [Datasets](#datasets)
- [Methodology](#methodology)
- [Installation](#installation)
- [Usage](#usage)
- [Results](#results)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The primary objective of this project is to explore the relationship between corporate inclusivity as measured by CEI ratings and the legislative environment around LGBTQ+ rights across different regions of the United States. This analysis helps understand trends, regional disparities, and potential areas for improvement in corporate inclusivity.

The research project is conducted as part of the fulfilment for the degree of Master of Data Science at University of Durham.

## Datasets

This repository contains the following datasets:

### 1. **Pride_Sponsorship_Data.csv**
   - **Description**: Contains information on corporate sponsorships for Pride events across various cities.
   - **Columns**:
     - `Company`: Name of the sponsoring company.
     - `City`: The city where the Pride event occurred.
     - `Year`: The year of the event.
     - `Sponsorship Level`: Level of sponsorship (e.g., Platinum, Gold, Silver).

### 2. **ACLU_Anti_LGBTQ_Data_2024.csv**
   - **Description**: This dataset includes the number of anti-LGBTQ bills in various states, classified by their legislative status.
   - **Columns**:
     - `State`: The U.S. state.
     - `Status`: The current status of the anti-LGBTQ bill (e.g., "Introduced", "Passed into Law", "Defeated").
     - `Issue`: The specific LGBTQ+ issue targeted by the bill (e.g., Healthcare Restrictions, Free Speech).
     - `Year`: The year when the bill was introduced or updated.

### 3. **CEI Data (2002-2023)**
   - **Description**: Contains annual CEI scores for companies from 2002 to 2023. The CEI measures corporate policies and practices pertinent to LGBTQ+ employees.
   - **Columns**:
     - `Employer`: Name of the company.
     - `State`: The state where the company is located.
     - `CEI Rating`: The Corporate Equality Index rating (0-100).
     - `Year`: Year of the rating.

## Methodology

1. **Time-Series Analysis**:
   - **ARIMA** and **ETS** models were applied to predict future trends in CEI ratings across four U.S. regions: Midwest, Northeast, South, and West.
   - Diagnostic plots (residuals, autocorrelation, and Q-Q plots) were created to validate the models' performance.

2. **Correlation Analysis**:
   - A Pearson correlation analysis was conducted to explore the relationship between the number of anti-LGBTQ bills and CEI ratings on a state level.

3. **Legislative Data Exploration**:
   - Bar plots were generated to visualize the number of anti-LGBTQ bills by state and their respective statuses (e.g., Passed into Law, Introduced, Defeated).

## Installation

To get started with this project, you'll need to install the following dependencies:

```bash
# Clone the repository
git clone https://github.com/your-username/cei-legislation-analysis.git
cd cei-legislation-analysis

# Install the required libraries
install.packages(c("ggplot2", "dplyr", "forecast", "usmap", "readr", "tidyr"))

```

Make sure you have **R** and **RStudio** (or another R environment) installed on your system.

## Usage

Once the necessary libraries are installed, you can run the R scripts provided in the repository to conduct the analysis and generate the visualizations.

```bash
# Example of loading a dataset
aclu_data <- read.csv("ACLU_Anti_LGBTQ_Data_2024.csv")

# Run an analysis (time series, correlation, or other analysis)
source("analysis_script.R")
```

### Example Outputs

- **Time-Series Forecasts**: Predicts future CEI ratings for different regions.
- **Correlation Analysis**: Examines the relationship between anti-LGBTQ bills and corporate inclusivity.
- **Bar Plots**: Shows the distribution of anti-LGBTQ bills by state and status.

## Results

The results of this project include:
- A detailed analysis of trends in corporate inclusivity.
- Regional comparisons of CEI ratings and legislative activity.
- Forecasted CEI ratings for different regions up to 2030.
- 
### Disclaimer

This research project and its analysis are subject to the following limitations:

1. **Data Gaps**: The datasets used in this analysis do not include all U.S. states or all companies. The ACLU Anti-LGBTQ dataset contains legislative data from 41 states and Puerto Rico, leaving some states unaccounted for. Additionally, the CEI data may not cover all companies within a state or region.

2. **Data Accuracy and Recency**: While efforts were made to ensure the accuracy of the data, some datasets may not reflect the most recent legislative changes or corporate inclusivity policies. The analysis focuses on data available up to 2023, and any legislative or corporate updates made after that time may not be captured in the results.

3. **Forecasting Limitations**: The ARIMA and ETS models used for forecasting CEI ratings rely on past data trends. These models assume that future trends will resemble historical patterns, which may not account for sudden social, political, or corporate shifts.

4. **Regional Generalization**: CEI ratings and legislative activities are aggregated at the regional level, which may mask important variations at the state or city level. This generalization could lead to overlooking unique conditions affecting specific locations.

5. **Correlation Does Not Imply Causation**: While correlation analyses were performed between CEI ratings and legislative activities, no causal relationships should be inferred. The results indicate the strength of the association but do not imply that anti-LGBTQ legislation directly causes changes in corporate inclusivity or vice versa.

6. **Interpretation Caution**: The results and visualizations produced in this project should be interpreted within the context of the known limitations and assumptions of the statistical models used.

By using or referencing this project, users acknowledge these limitations and understand that the findings presented here may not fully capture all aspects of corporate inclusivity or legislative activity related to LGBTQ+ rights.
