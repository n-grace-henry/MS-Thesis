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

ggplot(NPGO_annual, aes(x = Year, y = avg)) +
  geom_ribbon(aes(ymin = pmin(avg, 0), ymax = 0), fill = "blue", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(avg, 0)), fill = "red", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(min(NPGO_annual$Year), max(NPGO_annual$Year), by = 3)) +
  labs(title = "NPGO Anomaly Through Time",
       x = "Year",
       y = "NPGO") +
  theme_minimal()

# Write new csv
write.csv(NPGO_annual, file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO_tidy.csv")

