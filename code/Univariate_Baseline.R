# Load packages 
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(ggplot2)
library(dbplyr)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

# Set up time series object of PHE.mean from 1965-2022
phe_ts <- ts(data$PHE.mean, start = 1965, end = 2022, frequency = 1)



# Code from chatgpt: 
# Sample data frame
# Replace this with your actual data loading method
df <- data.frame(
  Year = c(1965, 1968, 1971, 1974, 1977, 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2022),
  PHE.mean = runif(20) # Replace this with your actual data
)

# Assume each year has three observations for three river systems
# Adding a river system column to illustrate
set.seed(123)
df$River.System <- rep(c("River1", "River2", "River3"), length.out = nrow(df))

# Create a complete sequence of years
years <- seq(1965, 2022, by = 1)

# Create a complete data frame
complete_df <- expand.grid(Year = years, River.System = c("River1", "River2", "River3"))

# Merge with the original data to insert NA values for missing data
merged_df <- left_join(complete_df, df, by = c("Year", "River.System"))

# Split the data by river system
river1_df <- merged_df %>% filter(River.System == "River1")
river2_df <- merged_df %>% filter(River.System == "River2")
river3_df <- merged_df %>% filter(River.System == "River3")

# Convert to time series objects
river1_ts <- ts(river1_df$PHE.mean, start = 1965, frequency = 1)
river2_ts <- ts(river2_df$PHE.mean, start = 1965, frequency = 1)
river3_ts <- ts(river3_df$PHE.mean, start = 1965, frequency = 1)

# Now you can use these time series objects in your model