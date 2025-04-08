# Load libraries
library(tidyverse)
library(patchwork)
library(zoo)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
raw <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/full.csv")

# Run anova to compare time periods 
# time periods are:
# 1965 - 1976
# 1977 - 2007 
# 2008 - 2022

# Trim df to be less crowded 
BB.TP <- data %>%
  select(Year, BB.tp)

# Calculate mean trophic position across all years
mean_tp <- mean(BB.TP$BB.tp, na.rm = TRUE)

# Add anomaly column to df





# Create anomaly column
data$TP.anomaly <- data$BB.tp - mean_tp

data$Period <- with(data, ifelse(Year >= 1965 & Year <= 1976, "1965-1976",
                                 ifelse(Year >= 1977 & Year <= 2007, "1977-2007",
                                        ifelse(Year >= 2008 & Year <= 2022, "2008-2022", NA))))
data$Period <- factor(data$Period, levels = c("1965-1976", "1977-2007", "2008-2022"))

anova_result <- aov(TP.anomaly ~ Period, data = data)
summary(anova_result)

# Post-hoc test
TukeyHSD(anova_result)


# Plotting
library(ggplot2)

ggplot(data, aes(x = Period, y = TP.anomaly)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  theme_minimal() +
  labs(title = "Trophic Position Anomalies by Time Period in Bristol Bay",
       y = "Trophic Position Anomaly", x = "Time Period")





