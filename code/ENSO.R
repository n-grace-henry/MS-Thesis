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
data_long <- data_long[data_long$Year >= "1965" & data_long$Year <= "2022",]

# Convert month to date
data_long <- data_long %>% 
  mutate(month_number = match(Month, month.abb))

# One value per year
data_long <- data_long %>% 
  group_by(Year) %>% 
  summarise(MEI = mean(MEI))

# Plot 
plot(x = data_long$Year,
     y = data_long$MEI,
     type ="l")

# Calculate anomaly
data_long <- data_long %>% 
  mutate(anomaly = MEI - mean(MEI))

# Plot with ggplot 
ggplot(data_long, aes(x = Year, y = anomaly, fill = anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() + 
  scale_fill_manual(values = c("TRUE" = "red2", "FALSE" = "blue3")) +
  labs(title = "ENSO Index",
       x = "Year",
       y = "ENSO") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  )

# Save as csv
write.csv(data_long, file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/ENSO_tidy.csv")



