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
complete_df <- expand.grid(Year = years, System = unique(data$System), Age = unique(data$Age))

# Merge with the original data to insert NA values for missing data
merged_df <- left_join(complete_df, data, by = c("Year", "System", "Age"))

# Split the data by river system and age class
Wood.2 <- merged_df %>% filter(System == "Wood") %>% filter(Age == 2)
Wood.3 <- merged_df %>% filter(System == "Wood") %>% filter(Age == 3)
Egegik.2 <- merged_df %>% filter(System == "Egegik") %>% filter(Age == 2)
Egegik.3 <- merged_df %>% filter(System == "Egegik") %>% filter(Age == 3)
Kvichak.2 <- merged_df %>% filter(System == "Kvichak") %>% filter(Age == 2)
Kvichak.3 <- merged_df %>% filter(System == "Kvichak") %>% filter(Age == 3)

# Convert to time series objects
Wood.2_ts <- ts(Wood.2$PHE.mean, start = 1965, frequency = 1)
Wood.3_ts <- ts(Wood.3$PHE.mean, start = 1965, frequency = 1)
Egegik.2_ts <- ts(Egegik.2$PHE.mean, start = 1965, frequency = 1)
Egegik.3_ts <- ts(Egegik.3$PHE.mean, start = 1965, frequency = 1)
Kvichak.2_ts <- ts(Kvichak.2$PHE.mean, start = 1965, frequency = 1)
Kvichak.3_ts <- ts(Kvichak.3$PHE.mean, start = 1965, frequency = 1)

#### Model Univariate State-Space Baseline ####


