setwd("~/Documents/GitHub/CSIA_lab_work/data")

# Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Load Data
data <- read.csv(file = "final/all_correct_final.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")

# Convert PDO to tidy format 
PDO_long <- gather(PDO, Month, Value, -Year)

# Sort by year
PDO_long <- arrange(PDO_long, Year)

# Convert month to date
PDO_long <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))

# Plot PDO from 1965-2022
PDO_subset <- subset(PDO_long, Year >= 1965 & Year <= 2022)
PDO_annual <- PDO_subset %>%
  group_by(Year) %>%
  summarise(Value = mean(Value, na.rm = TRUE))

plot(x = PDO_annual$Year,
     y = PDO_annual$Value,
     type = "l",
     xlab = "Year",
     ylab = "PDO")

# Write csv of tidy PDO data
write.csv(PDO_long, "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")


