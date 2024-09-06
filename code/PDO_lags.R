setwd("~/Documents/GitHub/CSIA_lab_work/data")

# Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Load Data
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

ggplot(PDO_annual, aes(x = Year, y = Value)) +
  geom_ribbon(aes(ymin = pmin(Value, 0), ymax = 0), fill = "blue", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(Value, 0)), fill = "red", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(min(PDO_annual$Year), max(PDO_annual$Year), by = 3)) +
  labs(title = "PDO Anomaly Through Time",
       x = "Year",
       y = "PDO") +
  theme_minimal()


# Write csv of tidy PDO data
write.csv(PDO_annual, "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")


