# Boston 311 Service Equity Analysis
# Analyzing response time disparities across neighborhoods

# Load required libraries
library(tidyverse)
library(lubridate)
library(sf)
library(randomForest)
library(caret)
library(scales)
library(knitr)
library(gridExtra)
library(leaflet)
library(rpart)
library(ranger)
library(car)
library(emmeans)

# Step 1: Data Loading and Preparation
# ====================================

# Load data from multiple years (2015-2019)
data_files <- list.files(pattern = "^\\d{4}\\.csv$")
boston_311 <- data_files %>%
  lapply(read.csv) %>%
  bind_rows()

# Do a quick check of the data
glimpse(boston_311)

# See what neighborhoods are in our dataset
unique_neighborhoods <- boston_311 %>%
  select(neighborhood) %>%
  distinct() %>%
  arrange(neighborhood)

# Show the list of neighborhoods
print(unique_neighborhoods)

# Step 2: Data Cleaning and Feature Engineering
# ============================================

# Define North and South Boston neighborhoods once - will be used throughout analysis
north_boston <- c("Charlestown", "East Boston", "North End", "Downtown", "Financial District", 
                  "Downtown / Financial District", "Beacon Hill", "West End", "Back Bay", 
                  "Fenway / Kenmore / Audubon Circle / Longwood", "Allston", "Brighton", 
                  "Allston / Brighton", "Fenway", "Kenmore", "Audubon Circle", "Longwood")

south_boston <- c("South Boston", "South Boston / South Boston Waterfront", 
                  "South Boston Waterfront", "South End", "Roxbury", "Dorchester", 
                  "Mattapan", "Greater Mattapan", "Jamaica Plain", "Mission Hill", 
                  "Roslindale", "West Roxbury", "Hyde Park", "Chestnut Hill")

# Function to classify neighborhoods into regions
classify_boston_region <- function(nbhd) {
  if (any(sapply(north_boston, function(n) grepl(n, nbhd, ignore.case = TRUE)))) {
    return("North Boston")
  } else if (any(sapply(south_boston, function(n) grepl(n, nbhd, ignore.case = TRUE)))) {
    return("South Boston")
  } else {
    return("Other")
  }
}

# Fix date formats and create useful features
boston_311_clean <- boston_311 %>%
  # Convert text dates to proper datetime objects
  mutate(
    open_dt = mdy_hm(open_dt),
    closed_dt = mdy_hm(closed_dt),
    sla_target_dt = mdy_hm(sla_target_dt)
  ) %>%
  # Remove records with missing dates
  filter(!is.na(open_dt), !is.na(closed_dt))

# Add many useful features for analysis
boston_311_features <- boston_311_clean %>%
  mutate(
    # Calculate how long it took to resolve each request (in hours)
    response_time_hours = as.numeric(difftime(closed_dt, open_dt, units = "hours")),
    
    # Was this request completed on time?
    on_time = ifelse(closed_dt <= sla_target_dt, 1, 0),
    
    # Break down the request time into useful components
    request_year = year(open_dt),
    request_month = month(open_dt),
    request_day = day(open_dt),
    request_hour = hour(open_dt),
    request_dow = wday(open_dt, label = TRUE),
    
    # Group months into seasons
    season = case_when(
      request_month %in% c(12, 1, 2) ~ "Winter",
      request_month %in% c(3, 4, 5) ~ "Spring",
      request_month %in% c(6, 7, 8) ~ "Summer",
      request_month %in% c(9, 10, 11) ~ "Fall"
    ),
    
    # Flag weekend requests
    is_weekend = ifelse(request_dow %in% c("Sat", "Sun"), 1, 0),
    
    # Fix empty neighborhood names
    neighborhood = ifelse(is.na(neighborhood), "Unknown", neighborhood),
    
    # Add region classification
    region = sapply(neighborhood, classify_boston_region)
  )

# Remove extreme outliers (too short or too long response times)
boston_311_features <- boston_311_features %>%
  filter(response_time_hours >= 0, response_time_hours <= 2000)  # Capping at ~3 months

# Step 3: Neighborhood Analysis Preparation
# =======================================

# Count how many requests came from each neighborhood
neighborhood_counts <- boston_311_features %>%
  count(neighborhood) %>%
  arrange(desc(n))

# Focus on top neighborhoods by request volume for simplified analysis
major_neighborhoods <- neighborhood_counts %>%
  head(10) %>%
  pull(neighborhood)

# Create a filtered dataset with just the major neighborhoods
boston_311_major <- boston_311_features %>%
  filter(neighborhood %in% major_neighborhoods)

# Step 4: Request Type Analysis
# ==========================

# Find the most common types of requests
request_type_counts <- boston_311_features %>%
  count(type) %>%
  arrange(desc(n))

# Get the top 10 most common request types
top_request_types <- request_type_counts %>%
  head(10) %>%
  pull(type)

# Step 5: Descriptive Statistics
# ============================

# Calculate overall statistics for all requests
overall_stats <- boston_311_features %>%
  summarize(
    total_requests = n(),
    avg_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    min_response_time = min(response_time_hours, na.rm = TRUE),
    max_response_time = max(response_time_hours, na.rm = TRUE),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100
  )

# Calculate statistics for each neighborhood
neighborhood_stats <- boston_311_features %>%
  group_by(neighborhood) %>%
  summarize(
    total_requests = n(),
    avg_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(total_requests))

# Show the overall stats and top neighborhood stats
print(overall_stats)
print(head(neighborhood_stats, 10))

# Step 6: Exploratory Data Visualization
# ===================================

# Define standard color scheme for consistency across all visualizations
region_colors <- c("North Boston" = "#000000", "South Boston" = "#9bd2d4")

# 1. Distribution of response times
# Filter to just North and South Boston regions for comparison
boston_311_regions <- boston_311_features %>%
  filter(region %in% c("North Boston", "South Boston"))

# Make sure North Boston is drawn last (on top) in overlapping plots
boston_311_regions$region <- factor(boston_311_regions$region, 
                                    levels = c("South Boston", "North Boston"))

# Create histogram comparing response times between regions
ggplot(boston_311_regions, aes(x = response_time_hours, fill = region)) +
  # Use position = "identity" to allow overlapping
  geom_histogram(position = "identity", alpha = 0.7, bins = 80) +
  scale_x_log10(
    breaks = c(1, 6, 12, 24, 48, 168, 336, 720), 
    labels = c("1hr", "6hrs", "12hrs", "1day", "2days", "1week", "2weeks", "1month")
  ) +
  scale_fill_manual(values = region_colors) +
  # Reverse the order of items in the legend to match visual prominence
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "Response Time (log scale)", 
    y = "Count",
    title = "Distribution of 311 Response Times in Boston", 
    subtitle = "Comparison between North Boston and South Boston neighborhoods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# 2. Response times by neighborhood (top 10)
# Calculate median response time for each neighborhood
neighborhood_rankings <- boston_311_features %>%
  group_by(neighborhood) %>%
  summarize(
    total_requests = n(),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter to neighborhoods with enough data
  filter(total_requests >= 500) %>%
  # Calculate rankings (1 = fastest)
  mutate(rank = rank(median_response_time)) %>%
  # Get top 10 neighborhoods by response time (lowest = best)
  arrange(median_response_time) %>%
  slice_head(n = 10)

# Create the ordered bar chart
ggplot(neighborhood_rankings, 
       aes(x = reorder(neighborhood, -rank), y = median_response_time)) +
  geom_col(fill = "#3a7ca5") +
  geom_text(aes(label = paste0("Rank #", rank), y = median_response_time + 1), 
            vjust = 0, color = "black", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Top 10 Boston Neighborhoods with Fastest Service Response Times",
    subtitle = "Ranked by median 311 request response time (2015-2019)",
    x = "Neighborhood",
    y = "Median Response Time (hours)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Boston 311 Service Equity Analysis
# Analyzing response time disparities across neighborhoods

# Load required libraries
library(tidyverse)
library(lubridate)
library(sf)
library(randomForest)
library(caret)
library(scales)
library(knitr)
library(gridExtra)
library(leaflet)
library(rpart)
library(ranger)

# Step 1: Data Loading and Preparation
# ====================================

# Load data from multiple years (2015-2019)
data_files <- list.files(pattern = "^\\d{4}\\.csv$")
boston_311 <- data_files %>%
  lapply(read.csv) %>%
  bind_rows()

# Do a quick check of the data
glimpse(boston_311)

# See what neighborhoods are in our dataset
unique_neighborhoods <- boston_311 %>%
  select(neighborhood) %>%
  distinct() %>%
  arrange(neighborhood)

# Show the list of neighborhoods
print(unique_neighborhoods)

#Case Status by year
ggplot(data_1,aes(x=year,fill= case_status))+ geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme_bw(base_size = 18)+ labs(title = "Case status by Year",)+ theme(plot.title = element_text(hjust = 0.5)) 

# Ticket closed in targeted time and overtime
ggplot(data_1,aes(x=year,fill= on_time))+ geom_bar(position = "dodge")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ theme_bw(base_size = 18)+ labs(title = "Ticket closed in targeted time Vs Overdue",)+ theme(plot.title = element_text(hjust = 0.5)) 

#Average Response time over the year  
data_3%>% filter(subject %in% c("Public Works Department", "Boston Water & Sewer Commission","Boston Police Department"))%>% ggplot(aes(x=as.numeric(year),y=Avg_response_time/60, color = subject))+ geom_line() + labs(x = "Year", y="Average response time", title = "Average Response time over the year") 

# Step 2: Data Cleaning and Feature Engineering
# ============================================

# Define North and South Boston neighborhoods once - will be used throughout analysis
north_boston <- c("Charlestown", "East Boston", "North End", "Downtown", "Financial District", 
                  "Downtown / Financial District", "Beacon Hill", "West End", "Back Bay", 
                  "Fenway / Kenmore / Audubon Circle / Longwood", "Allston", "Brighton", 
                  "Allston / Brighton", "Fenway", "Kenmore", "Audubon Circle", "Longwood")

south_boston <- c("South Boston", "South Boston / South Boston Waterfront", 
                  "South Boston Waterfront", "South End", "Roxbury", "Dorchester", 
                  "Mattapan", "Greater Mattapan", "Jamaica Plain", "Mission Hill", 
                  "Roslindale", "West Roxbury", "Hyde Park", "Chestnut Hill")

# Function to classify neighborhoods into regions
classify_boston_region <- function(nbhd) {
  if (any(sapply(north_boston, function(n) grepl(n, nbhd, ignore.case = TRUE)))) {
    return("North Boston")
  } else if (any(sapply(south_boston, function(n) grepl(n, nbhd, ignore.case = TRUE)))) {
    return("South Boston")
  } else {
    return("Other")
  }
}

# Fix date formats and create useful features
boston_311_clean <- boston_311 %>%
  # Convert text dates to proper datetime objects
  mutate(
    open_dt = mdy_hm(open_dt),
    closed_dt = mdy_hm(closed_dt),
    sla_target_dt = mdy_hm(sla_target_dt)
  ) %>%
  # Remove records with missing dates
  filter(!is.na(open_dt), !is.na(closed_dt))

# Add many useful features for analysis
boston_311_features <- boston_311_clean %>%
  mutate(
    # Calculate how long it took to resolve each request (in hours)
    response_time_hours = as.numeric(difftime(closed_dt, open_dt, units = "hours")),
    
    # Was this request completed on time?
    on_time = ifelse(closed_dt <= sla_target_dt, 1, 0),
    
    # Break down the request time into useful components
    request_year = year(open_dt),
    request_month = month(open_dt),
    request_day = day(open_dt),
    request_hour = hour(open_dt),
    request_dow = wday(open_dt, label = TRUE),
    
    # Group months into seasons
    season = case_when(
      request_month %in% c(12, 1, 2) ~ "Winter",
      request_month %in% c(3, 4, 5) ~ "Spring",
      request_month %in% c(6, 7, 8) ~ "Summer",
      request_month %in% c(9, 10, 11) ~ "Fall"
    ),
    
    # Flag weekend requests
    is_weekend = ifelse(request_dow %in% c("Sat", "Sun"), 1, 0),
    
    # Fix empty neighborhood names
    neighborhood = ifelse(is.na(neighborhood), "Unknown", neighborhood),
    
    # Add region classification
    region = sapply(neighborhood, classify_boston_region)
  )

# Remove extreme outliers (too short or too long response times)
boston_311_features <- boston_311_features %>%
  filter(response_time_hours >= 0, response_time_hours <= 2000)  # Capping at ~3 months

# Step 3: Neighborhood Analysis Preparation
# =======================================

# Count how many requests came from each neighborhood
neighborhood_counts <- boston_311_features %>%
  count(neighborhood) %>%
  arrange(desc(n))

# Focus on top neighborhoods by request volume for simplified analysis
major_neighborhoods <- neighborhood_counts %>%
  head(10) %>%
  pull(neighborhood)

# Create a filtered dataset with just the major neighborhoods
boston_311_major <- boston_311_features %>%
  filter(neighborhood %in% major_neighborhoods)

# Step 4: Request Type Analysis
# ==========================

# Find the most common types of requests
request_type_counts <- boston_311_features %>%
  count(type) %>%
  arrange(desc(n))

# Get the top 10 most common request types
top_request_types <- request_type_counts %>%
  head(10) %>%
  pull(type)

# Step 5: Descriptive Statistics
# ============================

# Calculate overall statistics for all requests
overall_stats <- boston_311_features %>%
  summarize(
    total_requests = n(),
    avg_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    min_response_time = min(response_time_hours, na.rm = TRUE),
    max_response_time = max(response_time_hours, na.rm = TRUE),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100
  )

# Calculate statistics for each neighborhood
neighborhood_stats <- boston_311_features %>%
  group_by(neighborhood) %>%
  summarize(
    total_requests = n(),
    avg_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(total_requests))

# Show the overall stats and top neighborhood stats
print(overall_stats)
print(head(neighborhood_stats, 10))

# Step 6: Exploratory Data Visualization
# ===================================

# Define standard color scheme for consistency across all visualizations
region_colors <- c("North Boston" = "#000000", "South Boston" = "#9bd2d4")

# 1. Distribution of response times
# Filter to just North and South Boston regions for comparison
boston_311_regions <- boston_311_features %>%
  filter(region %in% c("North Boston", "South Boston"))

# Make sure North Boston is drawn last (on top) in overlapping plots
boston_311_regions$region <- factor(boston_311_regions$region, 
                                    levels = c("South Boston", "North Boston"))

# Create histogram comparing response times between regions
ggplot(boston_311_regions, aes(x = response_time_hours, fill = region)) +
  # Use position = "identity" to allow overlapping
  geom_histogram(position = "identity", alpha = 0.7, bins = 80) +
  scale_x_log10(
    breaks = c(1, 6, 12, 24, 48, 168, 336, 720), 
    labels = c("1hr", "6hrs", "12hrs", "1day", "2days", "1week", "2weeks", "1month")
  ) +
  scale_fill_manual(values = region_colors) +
  # Reverse the order of items in the legend to match visual prominence
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "Response Time (log scale)", 
    y = "Count",
    title = "Distribution of 311 Response Times in Boston", 
    subtitle = "Comparison between North Boston and South Boston neighborhoods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# 2. Response times by neighborhood (top 10)
# Calculate median response time for each neighborhood
neighborhood_rankings <- boston_311_features %>%
  group_by(neighborhood) %>%
  summarize(
    total_requests = n(),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter to neighborhoods with enough data
  filter(total_requests >= 500) %>%
  # Calculate rankings (1 = fastest)
  mutate(rank = rank(median_response_time)) %>%
  # Get top 10 neighborhoods by response time (lowest = best)
  arrange(median_response_time) %>%
  slice_head(n = 10)

# Create the ordered bar chart
ggplot(neighborhood_rankings, 
       aes(x = reorder(neighborhood, -rank), y = median_response_time)) +
  geom_col(fill = "#3a7ca5") +
  geom_text(aes(label = paste0("Rank #", rank), y = median_response_time + 1), 
            vjust = 0, color = "black", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Top 10 Boston Neighborhoods with Fastest Service Response Times",
    subtitle = "Ranked by median 311 request response time (2015-2019)",
    x = "Neighborhood",
    y = "Median Response Time (hours)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# 3. On-time percentage by neighborhood (dot chart)
# Calculate on-time percentages for each neighborhood
neighborhood_ontime <- boston_311_features %>%
  group_by(neighborhood) %>%
  summarize(
    total_requests = n(),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100
  ) %>%
  # Only include neighborhoods with enough data
  filter(total_requests >= 1000) %>%
  # Add region classification explicitly since summarize() removes the region column
  mutate(region = sapply(neighborhood, classify_boston_region)) %>%
  # Filter to keep only North and South Boston
  filter(region != "Other") %>%
  arrange(desc(pct_on_time))

# Print region distribution to verify classification worked
print(table(neighborhood_ontime$region))

# Create the dot chart with explicit point colors
ggplot(neighborhood_ontime, aes(x = reorder(neighborhood, -pct_on_time), y = pct_on_time)) +
  # Use geom_point with explicit color mapping
  geom_point(aes(color = region), size = 4) +
  # Connect dots to baseline with adjusted starting point
  geom_segment(aes(xend = neighborhood, yend = 75, x = neighborhood), 
               color = "gray70", linewidth = 0.5) +
  # Add percentage labels
  geom_text(aes(label = sprintf("%.1f%%", pct_on_time)), 
            hjust = -0.3, size = 3) +
  # Set explicit colors for the regions
  scale_color_manual(name = "Region", values = region_colors) +
  # Set y-axis to zoom in on the 75-91% range
  scale_y_continuous(limits = c(75, 91), 
                     breaks = seq(75, 90, by = 5),
                     expand = expansion(mult = c(0, 0.15))) +
  # Flip coordinates
  coord_flip() +
  # Labels
  labs(
    x = "Neighborhood", 
    y = "Percent On-Time (%)",
    title = "On-Time Resolution Rate by Neighborhood",
    subtitle = "Comparison between North and South Boston neighborhoods (zoomed to 75-91% range)"
  ) +
  # Theme
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )

# 4. Response time by request type (top types)
# Calculate response times by request type and region
response_by_region <- boston_311_features %>%
  filter(type %in% top_request_types) %>%
  filter(region != "Other") %>%
  group_by(type, region) %>%
  summarize(
    total_requests = n(),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Ensure both regions exist for each type
  complete(type, region, fill = list(total_requests = 0, median_response_time = 0)) %>%
  # Get the maximum median response time for sorting
  group_by(type) %>%
  mutate(max_median = max(median_response_time)) %>%
  ungroup()

# Create a bar chart comparing response times by request type across regions
ggplot(response_by_region, aes(x = reorder(type, max_median), y = median_response_time, fill = region)) +
  # Use position_dodge with width = 0.8 and bar width = 0.8 to eliminate gaps
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  # Set colors for the two regions
  scale_fill_manual(values = region_colors) +
  # Add target reference lines
  geom_hline(yintercept = 24, linetype = "dashed", color = "darkgrey", alpha = 0.7) +  # 1 day
  geom_hline(yintercept = 48, linetype = "dotted", color = "darkgrey", alpha = 0.7) +  # 2 days
  # Flip coordinates for horizontal display
  coord_flip() +
  # Labels
  labs(
    x = "Request Type", 
    y = "Median Response Time (hours)",
    title = "Response Time by Request Type and Region",
    subtitle = "North Boston vs South Boston",
    fill = "Region"
  ) +
  # Theme customization
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    # Right-align the y-axis text
    axis.text.y = element_text(hjust = 1)
  )

# 5. Seasonal trends
seasonal_stats <- boston_311_features %>%
  group_by(season, request_year) %>%
  summarize(
    total_requests = n(),
    avg_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100
  )

ggplot(seasonal_stats, aes(x = season, y = avg_response_time, fill = factor(request_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Season", y = "Average Response Time (hours)", fill = "Year",
       title = "Seasonal Response Time Trends by Year")

# Step 7: Regression Analysis - Neighborhood Equity
# ==============================================

# Prepare data for regression model
model_data <- boston_311_features %>%
  # Select relevant variables
  select(
    response_time_hours, on_time, neighborhood, type, source,
    season, request_year, request_month, is_weekend, department
  ) %>%
  # Filter to records with complete data for modeling
  filter(
    !is.na(response_time_hours),
    !is.na(neighborhood),
    !is.na(type),
    !is.na(source),
    !is.na(department)
  )

# Convert categorical variables to factors
model_data <- model_data %>%
  mutate(
    neighborhood = as.factor(neighborhood),
    type = as.factor(type),
    source = as.factor(source),
    season = as.factor(season),
    request_year = as.factor(request_year),
    department = as.factor(department)
  )

# For simpler analysis, limit to major neighborhoods and request types
model_data_simplified <- model_data %>%
  filter(
    neighborhood %in% major_neighborhoods,
    type %in% top_request_types
  )

# Log-transform response time for better model fit
model_data_simplified$log_response_time <- log1p(model_data_simplified$response_time_hours)

# 1. Linear regression model
lm_model <- lm(log_response_time ~ neighborhood + type + source + 
                 season + request_year + is_weekend + department,
               data = model_data_simplified)

# Model summary
summary(lm_model)

# Extract neighborhood coefficients for equity analysis
neighborhood_effects <- broom::tidy(lm_model) %>%
  filter(grepl("^neighborhood", term)) %>%
  mutate(
    neighborhood = str_replace(term, "neighborhood", ""),
    effect = estimate
  ) %>%
  select(neighborhood, effect) %>%
  arrange(effect)

# Visualize neighborhood effects
ggplot(neighborhood_effects, aes(x = reorder(neighborhood, effect), y = effect)) +
  geom_col() +
  coord_flip() +
  labs(x = "Neighborhood", y = "Effect on Response Time (log scale)",
       title = "Neighborhood Effects on Response Time (Controlled for Other Factors)")

# Regression code for SLA target performance related to reason and type
mod<- lm(Response_time~on_time, data = data_1)  
print(mod)  
summary(mod) 

#Creating regression table  
library(stargazer)  
stargazer(mod, type="text", title="Regression Results", single.row=TRUE, ci=TRUE, ci.level=0.9)%>%  
  flextable(is.data.frame(mod))%>%  
  save_as_docx(path = "Doc14.docx")  

# Step 8: Machine Learning - Random Forest
# =====================================

# Prepare data for random forest (sample for efficiency)
set.seed(123)
rf_sample <- model_data_simplified %>%
  sample_n(min(10000, nrow(model_data_simplified)))

# Train random forest model using ranger
rf_model <- ranger(
  formula = log_response_time ~ neighborhood + type + source + 
    season + request_year + request_month + is_weekend + department,
  data = rf_sample,
  num.trees = 100,
  importance = "impurity"
)

# Extract variable importance
var_importance_df <- data.frame(
  variable = names(importance(rf_model)),
  importance = importance(rf_model)
) %>%
  arrange(desc(importance))

# Visualize variable importance
ggplot(var_importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(x = "Variable", y = "Importance (Impurity)",
       title = "Variable Importance in Predicting Response Time")

# Step 10: Year-over-Year Equity Analysis
# ====================================

# First, calculate yearly stats for each neighborhood
yearly_neighborhood_stats <- boston_311_features %>%
  group_by(neighborhood, request_year) %>%
  summarize(
    total_requests = n(),
    avg_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    pct_on_time = mean(on_time, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Calculate city-wide averages for each year
yearly_city_averages <- boston_311_features %>%
  group_by(request_year) %>%
  summarize(
    city_avg_response_time = mean(response_time_hours, na.rm = TRUE),
    .groups = "drop"
  )

# Get the top 10 neighborhoods by total request volume
top_neighborhoods <- boston_311_features %>%
  count(neighborhood) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  pull(neighborhood)

# Create the equity_trends data frame with disparity index for top 10 neighborhoods
equity_trends <- yearly_neighborhood_stats %>%
  # Filter to include only top 10 neighborhoods
  filter(neighborhood %in% top_neighborhoods) %>%
  # Join with city averages
  left_join(yearly_city_averages, by = "request_year") %>%
  # Calculate disparity index (ratio of neighborhood to city average)
  mutate(disparity_index = avg_response_time / city_avg_response_time) %>%
  # Add region classification for potential additional analysis
  mutate(region = sapply(neighborhood, classify_boston_region))

# Identify top 3 neighborhoods with highest disparity index
top_disparity_neighborhoods <- equity_trends %>%
  group_by(neighborhood) %>%
  summarize(avg_disparity = mean(disparity_index)) %>%
  arrange(desc(avg_disparity)) %>%
  head(3) %>%
  pull(neighborhood)

# Create a modified dataset for the visualization with highlight flag
equity_trends_viz <- equity_trends %>%
  mutate(highlight = neighborhood %in% top_disparity_neighborhoods)

# Create a better color palette for first visualization
highlight_colors <- c("#E41A1C", "#377EB8", "#4DAF4A")  # Red, Blue, Green
names(highlight_colors) <- top_disparity_neighborhoods

# Improve the visualization with a cleaner design - highlight top 3 neighborhoods
ggplot(equity_trends_viz, 
       aes(x = factor(request_year), y = disparity_index, 
           group = neighborhood)) +
  # Add a subtle grid that doesn't overwhelm the data
  theme_minimal() +
  # Add the light gray lines for non-highlighted neighborhoods
  geom_line(data = subset(equity_trends_viz, !highlight),
            color = "lightgray", size = 0.8) +
  geom_point(data = subset(equity_trends_viz, !highlight),
             color = "lightgray", size = 2, shape = 21, fill = "white", stroke = 0.8) +
  # Add the colored lines for highlighted neighborhoods
  geom_line(data = subset(equity_trends_viz, highlight),
            aes(color = neighborhood), size = 1.2) +
  geom_point(data = subset(equity_trends_viz, highlight),
             aes(color = neighborhood), size = 3, shape = 21, fill = "white", stroke = 1.5) +
  # Set the color scale for highlighted neighborhoods
  scale_color_manual(values = highlight_colors) +
  # Customize the grid
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    text = element_text(family = "sans")
  ) +
  # Reference line
  geom_hline(yintercept = 1, linetype = "dashed", color = "#000000", size = 0.8) +
  # Highlight the most concerning neighborhoods with annotations
  geom_text(
    data = subset(equity_trends_viz, request_year == max(request_year) & highlight),
    aes(label = neighborhood, x = factor(request_year), y = disparity_index + 0.03, color = neighborhood),
    hjust = -0.1, size = 3.5, fontface = "bold"
  ) +
  # Better axis and title formatting
  labs(
    x = "Year", 
    y = "Disparity Index", 
    title = "Trends in Neighborhood Response Time Disparities",
    subtitle = "Values above 1 indicate longer response times than the city average\nHighlighting the 3 neighborhoods with highest average disparities",
    color = "Top 3 Neighborhoods"
  ) +
  # Extend x-axis slightly to make room for annotations
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) +
  # Add direct labels for key neighborhoods instead of relying solely on the legend
  guides(color = guide_legend(ncol = 1, byrow = TRUE))

# Define region colors for the second visualization
region_colors_facet <- c("North Boston" = "#000000", "South Boston" = "#9bd2d4", "Other" = "#cccccc")

# Alternative version with facets to completely eliminate overlapping lines
# Color lines by region (North Boston vs South Boston)
ggplot(equity_trends, 
       aes(x = factor(request_year), y = disparity_index, group = 1)) +
  # Create separate small multiples for each neighborhood
  facet_wrap(~ neighborhood, scales = "free_y", ncol = 3) +
  # Use a clean theme
  theme_minimal() +
  # Refine the theme
  theme(
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  # Add lines for each neighborhood, colored by region
  geom_line(aes(color = region), size = 1.2) +
  # Add points
  geom_point(aes(color = region), size = 3) +
  # Color by region
  scale_color_manual(values = region_colors_facet) +
  # Reference line at 1.0
  geom_hline(yintercept = 1, linetype = "dashed", color = "#000000", size = 0.5) +
  # Better labeling
  labs(
    x = "Year", 
    y = "Disparity Index", 
    title = "Neighborhood Response Time Disparities (2015-2019)",
    subtitle = "Values above 1 indicate longer response times than city average\nLines colored by region (North vs South Boston)",
    color = "Region"
  )

# Step 11: Summarizing Findings
# ==========================

# Create a neighborhood equity report
equity_report <- yearly_neighborhood_stats %>%
  group_by(neighborhood) %>%
  summarize(
    total_requests_all_years = sum(total_requests),
    avg_response_time_all_years = mean(avg_response_time),
    trend = case_when(
      cor(as.numeric(request_year), avg_response_time, use = "complete.obs") > 0.5 ~ "Worsening",
      cor(as.numeric(request_year), avg_response_time, use = "complete.obs") < -0.5 ~ "Improving",
      TRUE ~ "Stable"
    )
  ) %>%
  arrange(desc(total_requests_all_years))

# Print summary report
print(head(equity_report, n=30))

# Step 12: ANOVA Analysis
# ==============================

# ANOVA 1: Compare response times between North and South Boston regions
# --------------------------------------------------------------------

# First, check for normality in the response time distribution
# Since response times are often skewed, we'll use log-transformed values
boston_311_regions$log_response_time <- log1p(boston_311_regions$response_time_hours)

# Set up model for ANOVA by region
region_anova <- aov(log_response_time ~ region, data = boston_311_regions)

# Run ANOVA and print results
region_anova_summary <- summary(region_anova)
print("ANOVA Results - Response Time by Region:")
print(region_anova_summary)

# Calculate and print mean response times by region
region_means <- boston_311_regions %>%
  group_by(region) %>%
  summarize(
    mean_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    log_mean_response_time = mean(log_response_time, na.rm = TRUE),
    n = n()
  )
print("Mean Response Times by Region:")
print(region_means)

# Visualize ANOVA results
ggplot(boston_311_regions, aes(x = region, y = log_response_time, fill = region)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = region_colors) +
  labs(
    title = "ANOVA: Response Time Differences Between Regions",
    subtitle = "Using log-transformed response times for better normality",
    x = "Region",
    y = "Log(Response Time + 1) in hours"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# ANOVA 2: Compare response times across all major neighborhoods
# ------------------------------------------------------------

# Prepare data for neighborhood ANOVA
neighborhood_anova_data <- boston_311_features %>%
  filter(neighborhood %in% major_neighborhoods) %>%
  mutate(log_response_time = log1p(response_time_hours))

# Set up model for ANOVA by neighborhood
neighborhood_anova <- aov(log_response_time ~ neighborhood, data = neighborhood_anova_data)

# Run ANOVA and print results
neighborhood_anova_summary <- summary(neighborhood_anova)
print("ANOVA Results - Response Time by Neighborhood:")
print(neighborhood_anova_summary)

# If ANOVA is significant, run Tukey's HSD post-hoc test to compare neighborhoods
if (neighborhood_anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_results <- TukeyHSD(neighborhood_anova)
  print("Post-hoc Tukey HSD Test Results (showing only significant comparisons):")
  # Extract only significant differences (p < 0.05)
  significant_diffs <- which(tukey_results$neighborhood[,4] < 0.05)
  if (length(significant_diffs) > 0) {
    print(tukey_results$neighborhood[significant_diffs, ])
  } else {
    print("No significant pairwise differences found")
  }
}

# Calculate and print mean response times by neighborhood
neighborhood_means <- neighborhood_anova_data %>%
  group_by(neighborhood) %>%
  summarize(
    mean_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    log_mean_response_time = mean(log_response_time, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(mean_response_time)
print("Mean Response Times by Neighborhood:")
print(neighborhood_means)

# Visualize neighborhood ANOVA results
ggplot(neighborhood_anova_data, 
       aes(x = reorder(neighborhood, log_response_time, FUN = mean), 
           y = log_response_time, fill = neighborhood)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  labs(
    title = "ANOVA: Response Time Differences Between Neighborhoods",
    subtitle = "Using log-transformed response times for better normality",
    x = "Neighborhood",
    y = "Log(Response Time + 1) in hours"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# ANOVA 3: Compare response times across seasons
# --------------------------------------------

# Prepare data for seasonal ANOVA
season_anova_data <- boston_311_features %>%
  mutate(log_response_time = log1p(response_time_hours))

# Create a factor with appropriate order for seasons
season_anova_data$season <- factor(season_anova_data$season, 
                                   levels = c("Winter", "Spring", "Summer", "Fall"))

# Set up model for ANOVA by season
season_anova <- aov(log_response_time ~ season, data = season_anova_data)

# Run ANOVA and print results
season_anova_summary <- summary(season_anova)
print("ANOVA Results - Response Time by Season:")
print(season_anova_summary)

# If ANOVA is significant, run Tukey's HSD post-hoc test to compare seasons
if (season_anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  season_tukey <- TukeyHSD(season_anova)
  print("Post-hoc Tukey HSD Test Results for Seasons:")
  print(season_tukey)
}

# Calculate and print mean response times by season
season_means <- season_anova_data %>%
  group_by(season) %>%
  summarize(
    mean_response_time = mean(response_time_hours, na.rm = TRUE),
    median_response_time = median(response_time_hours, na.rm = TRUE),
    log_mean_response_time = mean(log_response_time, na.rm = TRUE),
    n = n()
  )
print("Mean Response Times by Season:")
print(season_means)

# Visualize season ANOVA results
ggplot(season_anova_data, aes(x = season, y = log_response_time, fill = season)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "ANOVA: Response Time Differences Between Seasons",
    subtitle = "Using log-transformed response times for better normality",
    x = "Season",
    y = "Log(Response Time + 1) in hours"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

# ANOVA 4: Two-way ANOVA for interaction between region and season
# --------------------------------------------------------------

# Prepare data for two-way ANOVA
twoway_anova_data <- boston_311_regions %>%
  mutate(
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )

# Set up model for two-way ANOVA
twoway_anova <- aov(log_response_time ~ region * season, data = twoway_anova_data)

# Run ANOVA and print results
twoway_anova_summary <- summary(twoway_anova)
print("Two-Way ANOVA Results - Response Time by Region and Season:")
print(twoway_anova_summary)

# If interaction is significant, analyze simple main effects
if (twoway_anova_summary[[1]][["Pr(>F)"]][3] < 0.05) {  # Check interaction p-value
  print("Significant interaction found. Analyzing simple main effects:")
  
  # Calculate mean response times for each region-season combination
  interaction_means <- twoway_anova_data %>%
    group_by(region, season) %>%
    summarize(
      mean_response_time = mean(response_time_hours, na.rm = TRUE),
      log_mean_response_time = mean(log_response_time, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  print("Mean Response Times by Region and Season:")
  print(interaction_means)
  
  # Create an interaction plot
  ggplot(interaction_means, 
         aes(x = season, y = mean_response_time, group = region, color = region)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = region_colors) +
    labs(
      title = "Interaction Effect Between Region and Season on Response Time",
      x = "Season",
      y = "Mean Response Time (hours)",
      color = "Region"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top"
    )
}

# Add ANOVA results to the comprehensive report
anova_findings <- data.frame(
  Comparison = c(
    "North vs South Boston Regions",
    "Top 10 Neighborhoods",
    "Seasonal Effects",
    "Region-Season Interaction"
  ),
  "F-statistic" = c(
    round(region_anova_summary[[1]]["F value"][1,], 2),
    round(neighborhood_anova_summary[[1]]["F value"][1,], 2),
    round(season_anova_summary[[1]]["F value"][1,], 2),
    round(twoway_anova_summary[[1]]["F value"][3,], 2)  # Interaction term
  ),
  "p-value" = c(
    format.pval(region_anova_summary[[1]]["Pr(>F)"][1,], digits = 3),
    format.pval(neighborhood_anova_summary[[1]]["Pr(>F)"][1,], digits = 3),
    format.pval(season_anova_summary[[1]]["Pr(>F)"][1,], digits = 3),
    format.pval(twoway_anova_summary[[1]]["Pr(>F)"][3,], digits = 3)  # Interaction term
  ),
  Significant = c(
    ifelse(region_anova_summary[[1]]["Pr(>F)"][1,] < 0.05, "Yes", "No"),
    ifelse(neighborhood_anova_summary[[1]]["Pr(>F)"][1,] < 0.05, "Yes", "No"),
    ifelse(season_anova_summary[[1]]["Pr(>F)"][1,] < 0.05, "Yes", "No"),
    ifelse(twoway_anova_summary[[1]]["Pr(>F)"][3,] < 0.05, "Yes", "No")  # Interaction term
  )
)

print("Summary of ANOVA Results:")
print(anova_findings)

# Check assumptions for ANOVA
# Levene's test for homogeneity of variance
levene_region <- car::leveneTest(log_response_time ~ region, data = boston_311_regions)
levene_neighborhood <- car::leveneTest(log_response_time ~ neighborhood, data = neighborhood_anova_data)
levene_season <- car::leveneTest(log_response_time ~ season, data = season_anova_data)

print("Levene's Test for Homogeneity of Variance:")
print("Region ANOVA:")
print(levene_region)
print("Neighborhood ANOVA:")
print(levene_neighborhood)
print("Season ANOVA:")
print(levene_season)

# If any Levene's test is significant (p < 0.05), assumptions are violated
# In that case, we can use Welch's ANOVA which does not assume equal variances
if (levene_region$`Pr(>F)`[1] < 0.05) {
  welch_region <- oneway.test(log_response_time ~ region, data = boston_311_regions, var.equal = FALSE)
  print("Welch's ANOVA for Region (robust to variance heterogeneity):")
  print(welch_region)
}

if (levene_neighborhood$`Pr(>F)`[1] < 0.05) {
  welch_neighborhood <- oneway.test(log_response_time ~ neighborhood, data = neighborhood_anova_data, var.equal = FALSE)
  print("Welch's ANOVA for Neighborhood (robust to variance heterogeneity):")
  print(welch_neighborhood)
}

if (levene_season$`Pr(>F)`[1] < 0.05) {
  welch_season <- oneway.test(log_response_time ~ season, data = season_anova_data, var.equal = FALSE)
  print("Welch's ANOVA for Season (robust to variance heterogeneity):")
  print(welch_season)
}

