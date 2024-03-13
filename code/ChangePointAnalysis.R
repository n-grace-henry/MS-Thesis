setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv(file = "data.csv")

#visualize
library(ggplot2)
ggplot(data, aes(x = Year,
                 y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7)

# Getting the data in a format that will work to create a time series object
# Convert the "Year" column to a factor to ensure correct ordering
data$Year <- factor(data$Year, levels = rev(unique(data$Year)))

# Pivot the data to wide format to create separate columns for each river and age combination
library(tidyr)
data_wide <- spread(data, System, PHE.mean)

# Create separate time series objects for each river and age combination
ts_river1_age2 <- ts(data_wide$age[data_wide$River == "River1" & data_wide$age == 2], 
                     start = 2022, frequency = 1)
ts_river1_age3 <- ts(data_wide$age[data_wide$River == "River1" & data_wide$age == 3], 
                     start = 2022, frequency = 1)
ts_river2_age2 <- ts(data_wide$age[data_wide$River == "River2" & data_wide$age == 2], 
                     start = 2022, frequency = 1)
ts_river2_age3 <- ts(data_wide$age[data_wide$River == "River2" & data_wide$age == 3], 
                     start = 2022, frequency = 1)
ts_river3_age2 <- ts(data_wide$age[data_wide$River == "River3" & data_wide$age == 2], 
                     start = 2022, frequency = 1)
ts_river3_age3 <- ts(data_wide$age[data_wide$River == "River3" & data_wide$age == 3], 
                     start = 2022, frequency = 1)








#load packages
library(changepoint)

# Remove rows with NAs
data <- na.omit(data)

# Reverse the order of rows in the data frame so that the oldest year is at the top
data <- data[order(data$Year, decreasing = TRUE), ]

# Convert data to a time series object
ts_data <- ts(data$PHE.mean, start = data$Year[length(data$Year)], end = data$Year[1])

# Perform change point analysis using the 'cpt.mean' function
cpt_result <- cpt.mean(ts_data)

# Plot the change point analysis results
plot(cpt_result, cpt.col = "blue")

summary(cpt_result)
