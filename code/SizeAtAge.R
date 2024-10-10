# Load libraries 
library(ggplot2)
library(dplyr)

# Load data
size <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA_covariates_2024.csv")
str(size)
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/Ohlberger.csv")

# Format size df
size <- size[!size$Year %in% c(2023, 2024),]
size <- size %>% select(Year, Pink_Salmon_lag1, Total_Run, X2.ocean_mass)
size <- size %>% select(-X, -X.1, -X3.ocean_mass, -Summer_SST_lag1)

# Format data df
data <- data %>% select(year, total_return, pink_tot_num, chum_tot_num)





colnames(data) <- c("Year", "Wood", "Kvichak", "Egegik")

# Update Ohlberger data with Woodard data


# Plot updated pink returns 



# Save new data frame as csv for analysis in other script 



