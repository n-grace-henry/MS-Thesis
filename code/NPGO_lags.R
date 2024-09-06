# Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Load Data
NPGO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO.csv")

# NPGO average per year
NPGO_annual <- NPGO %>%
  group_by(Year) %>%
  summarise(avg = mean(NPGO))

# Take only years of interest
NPGO_annual <- NPGO_annual[NPGO_annual$Year >= 1965 & NPGO_annual$Year <= 2022,]

# Plot 
plot(NPGO_annual, type = "l")

# Convert to time series 
ts.NPGO <- ts(NPGO_annual[,2],
              start = NPGO_annual[1,1],
              end = NPGO_annual[72,1],
              frequency = 1)

# Write new csv
write.csv(NPGO_annual, file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO_tidy.csv")

