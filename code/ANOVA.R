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
BB.TP$TP.anomaly <- data$BB.tp - mean_tp

# Assign time period 
BB.TP$Period <- with(data, ifelse(Year >= 1965 & Year <= 1976, "1965-1976",
                                 ifelse(Year >= 1977 & Year <= 2007, "1977-2007",
                                        ifelse(Year >= 2008 & Year <= 2022, "2008-2022", NA))))
# Period as factor 
BB.TP$Period <- factor(BB.TP$Period, levels = c("1965-1976", "1977-2007", "2008-2022"))

# Run ANOVA 
anova_result <- aov(TP.anomaly ~ Period, data = BB.TP)
summary(anova_result)
# Sum Sq: the variability in trophic position anomalies explained by Period is 0.5527
# F value: ratio of between-group to within-group variation. High F-value indicates that the group means are very different
# p-value: super small, meaning the likelihood of seeing this much difference between group means by chance is extremely low 
# there is a statistically sig difference between group means 

# Post-hoc test
TukeyHSD(anova_result)
# 1977-2007 was significantly lower than 1965-1976
# no significant difference between 2008-2022 and 1965-1976
# Tp was signification higher in 2008-2022 than in 1977-2007

# Plotting
ggplot(BB.TP, aes(x = Period, y = TP.anomaly)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  theme_minimal() +
  labs(title = "Trophic Position Anomalies by Time Period in Bristol Bay",
       y = "Trophic Position Anomaly", x = "Time Period")

