# Load libraries 
library(ggplot2)
library(dplyr)

# Load data
size <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA_covariates_2024.csv")
str(size)
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/Ohlberger.csv")
SaA_BB <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA/Mean_SaA_esc_ocean2s_by_year_BB_wide.csv")
SaA_system <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA/Mean_SaA_esc_ocean2s_by_year_each_system.csv")
SaA_system_avg <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA/Mean_SaA_esc_ocean2s_by_year_across_systems.csv")

# Format size df
size <- size[!size$Year %in% c(2023, 2024),]
size <- size %>% select(Year, Total_Run, Pink_Salmon_lag1)
colnames(size) <- c("Year", "Total", "Pink_lag1")

# Format data df
data <- data %>% select(year, total_return, pink_tot_num)
colnames(data) <- c("Year", "Total", "Pink")

# Unlag size df
size$Pink <- c(size$Pink_lag1[-1], NA)

# Lag data df
data$Pink_lag1 <- c(NA, data$Pink[-nrow(data)])

# Reorder size df 
size <- size %>% select(Year, Total, Pink, Pink_lag1)

# Add extra years from size to the bottom of data df
extrayr <- size[!size$Year %in% data$Year,]
data <- rbind(data, extrayr)

# Save csv of pinks 
write.csv(data, file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/Pink.csv")

# Plot pink returns 
ggplot(data, aes(x = Year, y = Pink)) +
  geom_line() +
  geom_point() +
  labs(title = "Pink Salmon Returns", x = "Year", y = "Number of Pink Salmon")

# Plot size at age through time 
ggplot(data = SaA_system_avg, aes(year, mean_SaA)) +
  geom_line() +
  geom_smooth()



