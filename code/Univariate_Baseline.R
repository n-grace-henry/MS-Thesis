# Load packages 
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(ggplot2)
library(dbplyr)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

#### Create time series objects ####

# Create a complete sequence of years
years <- seq(1965, 2022, by = 1)

# Create a complete data frame for each river system
complete_df <- expand.grid(Year = years, System = unique(data$System))

# Merge with the original data to insert NA values for missing data
merged_df <- left_join(complete_df, data, by = c("Year", "System"))

# Split the data by river system
Wood <- merged_df %>% filter(System == "Wood")
Egegik <- merged_df %>% filter(System == "Egegik")
Kvichak <- merged_df %>% filter(System == "Kvichak")

# Convert to time series objects
Wood_ts <- ts(river1_df$PHE.mean, start = 1965, frequency = 1)
Egegik_ts <- ts(river2_df$PHE.mean, start = 1965, frequency = 1)
Kvichak_ts <- ts(river3_df$PHE.mean, start = 1965, frequency = 1)

# Print the time series objects to verify
print(river1_ts)
print(river2_ts)
print(river3_ts)


phe_ts <- ts(data$PHE.mean, start = 1965, end = 2022, frequency = 1)

