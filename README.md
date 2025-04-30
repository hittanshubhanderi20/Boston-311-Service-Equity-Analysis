# Boston 311 Service Equity Analysis

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![R Version](https://img.shields.io/badge/R-v4.1.0+-blue.svg)

## Overview

This project investigates geographic disparities in Boston's 311 non-emergency service request system, with a focus on response time inequities between neighborhoods. Using five years (2015-2019) of Boston's open 311 service request data, we analyze response time patterns, develop predictive models, and identify factors contributing to service delivery disparities across the city.

## Key Findings

- **Geographic Disparities**: South Boston neighborhoods experience response times that are on average 12.5% longer than North Boston neighborhoods (126 hours vs. 112 hours).

- **Predictive Power**: Our Random Forest model identified service request type, source of request, and year as the strongest predictors of response time, with the full model explaining nearly 50% of the variation.

- **Neighborhood Effects**: After controlling for other factors, some neighborhoods consistently experience longer response times, with Jamaica Plain showing the longest delays and Hyde Park showing the fastest improvements.

- **Seasonal Impact**: Winter months show significantly longer response times (128 hours) compared to Fall (105 hours), with the seasonal effect more pronounced in South Boston.

- **Request Type Variation**: Service request types like Sign Repair, Snow Plowing, and Bulk Item Pickup have much longer response times compared to Parking Enforcement and Street Cleaning requests.

## Data

The dataset consists of 311 service requests from Boston's Open Data Portal spanning 2015-2019. Each record includes:

- Request open and close timestamps
- Request type and description
- Neighborhood and location
- Responsible department
- Service Level Agreement (SLA) target date
- Request source (app, website, phone, etc.)

## Methodology

### 1. Data Preparation

- Combined multiple years of data into a unified dataset
- Converted text dates to proper datetime objects
- Created new features including response time, on-time flag, and seasonal indicators
- Classified neighborhoods into North Boston and South Boston regions

### 2. Exploratory Data Analysis

We conducted extensive exploratory analysis to understand patterns in the data:

- Distributions of response times across neighborhoods and regions
- On-time resolution rates by neighborhood
- Response time by request type and neighborhood
- Seasonal trends and year-over-year changes

### 3. Statistical Modeling

- **Linear Regression**: Examined neighborhood effects while controlling for other factors
- **Random Forest**: Identified the most important predictors of response time
- **ANOVA**: Compared response times across regions, neighborhoods, and seasons
- **Trend Analysis**: Tracked changes in service equity over the five-year period

## Key Visualizations

### Response Time Distribution by Region
![Response Time Distribution](screenshot1.png)

This histogram shows the distribution of response times for North vs South Boston neighborhoods on a logarithmic scale. South Boston neighborhoods (teal) show consistently longer response times, particularly in the 1-day to 1-week range.

### Top Predictors of Response Time
![Variable Importance](screenshot2.png)

The Random Forest model identified request type as the most important predictor of response time, followed by source and request year. These three variables together account for the majority of predictive power.

### Response Time by Request Type and Region
![Response by Type](screenshot3.png)

This chart compares median response times between North and South Boston across different request types. South Boston consistently experiences longer response times across most request types.

### Neighborhood Response Time Disparities
![Disparity Trends](screenshot4.png)

This visualization tracks neighborhood response time disparities over the five-year period. The three neighborhoods with the highest disparities (Hyde Park, Dorchester, and Jamaica Plain) are highlighted, showing persistent inequities.

## Technologies Used

- **R**: Primary programming language for data analysis
- **tidyverse**: Data manipulation and visualization
- **lubridate**: Date and time handling
- **ranger/randomForest**: Machine learning modeling
- **sf**: Spatial data handling
- **ggplot2**: Data visualization
- **caret**: Machine learning workflow
- **car/emmeans**: Statistical analysis

## How to Use This Repository

### Prerequisites

- R (version 4.1.0 or higher)
- Required R packages (listed in the scripts)

### Running the Analysis

1. Clone this repository
2. Download the Boston 311 data files from [Boston's Open Data Portal](https://data.boston.gov/dataset/311-service-requests)
3. Place the CSV files in the project directory (named as `2015.csv`, `2016.csv`, etc.)
4. Run the `project1_final_file.R` script to reproduce the full analysis

```r
# Example code to run the analysis
source("project1_final_file.R")
```

## Project Structure

- `project1_final_file.R`: Main analysis script with all data processing, visualization, and modeling
- `project1.R`: Development version with additional exploratory code
- `Initial_Analysis_Report.pdf`: Project documentation and findings

## Authors

- Hittanshu Mansukhbhai Bhanderi
- Aditi Anil Shinde
- Unnati Kaur Hunjan

## Acknowledgments

- Data provided by the City of Boston's Open Data Portal
- Northeastern University, ALY6015- Intermediate Analytics

## License

This project is licensed under the MIT License - see the LICENSE file for details.
