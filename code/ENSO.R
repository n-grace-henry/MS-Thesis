# Data from: https://psl.noaa.gov/gcos_wgsp/Timeseries/Nino4/
# Reference: Rayner N. A., D. E. Parker, E. B. Horton, C. K. Folland, 
# L. V. Alexander, D. P. Rowell, E. C. Kent, A. Kaplan, Global analyses 
# of sea surface temperature, sea ice, and night marine air temperature 
# since the late nineteenth century, J. Geophys. Res., 108 (D14), 4407, 
# doi:10.1029/2002JD002670, 2003.

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Load data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/Environmental/ENSO.csv")

# Convert to long format 
data_long <- data %>% 
  pivot_longer(cols = -1, names_to = "Month", values_to = "MEI")

# Grab years 1964-2023
data_long <- data_long[data_long$Year >= "1964" & data_long$Year <= "2023",]

# Convert month to date
data_long <- data_long %>% 
  mutate(month_number = match(Month, month.abb))

# Plot 
plot(x = data_long$Year,
     y = data_long$MEI,
     type ="l")
