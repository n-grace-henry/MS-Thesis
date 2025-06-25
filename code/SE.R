# Load libraries
library(tidyverse)
library(patchwork)
library(zoo)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
raw <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/full.csv")

# time periods are:
# 1965 - 1976
# 1977 - 2007 
# 2008 - 2022

# Trim df to be less busy 
BB.TP <- data %>%
  select(Year, BB.tp, tp.SE)

# Calculate the standard deviation of each time period 
p1.sd <- BB.TP %>%
  filter(Year >= 1965 & Year <= 1976) %>%
  summarise(sd = sd(BB.tp, na.rm = TRUE))
p2.sd <- BB.TP %>%
  filter(Year >= 1977 & Year <= 2007) %>%
  summarise(sd = sd(BB.tp, na.rm = TRUE))
p3.sd <- BB.TP %>%
  filter(Year >= 2008 & Year <= 2022) %>%
  summarise(sd = sd(BB.tp, na.rm = TRUE))

# Calculate the mean of each time period
p1.mean <- BB.TP %>%
  filter(Year >= 1965 & Year <= 1976) %>%
  summarise(mean = mean(BB.tp, na.rm = TRUE))
p2.mean <- BB.TP %>%
  filter(Year >= 1977 & Year <= 2007) %>%
  summarise(mean = mean(BB.tp, na.rm = TRUE))
p3.mean <- BB.TP %>%
  filter(Year >= 2008 & Year <= 2022) %>%
  summarise(mean = mean(BB.tp, na.rm = TRUE))

# SE of each time period as compared to long term mean 
p1.se <- (p1.mean$mean - overall_mean)/overall_sd
p2.se <- (overall_mean - p2.mean$mean)/overall_sd
p3.se <- (p3.mean$mean - overall_mean)/overall_sd

# Step 1: Calculate long-term mean TP
overall_mean <- round(mean(BB.TP$BB.tp, na.rm = TRUE), digit = 1)
overall_sd <- round(sd(BB.TP$BB.tp, na.rm = TRUE), digits = 2)

# Step 2: Define a function to calculate stats for each period
period_stats <- function(df_period) {
  # Weighted mean TP
  w_mean <- sum(df_period$BB.tp / df_period$tp.SE^2) / sum(1 / df_period$tp.SE^2)
  
  # Pooled SE
  pooled_se <- sqrt(1 / sum(1 / df_period$tp.SE^2))
  
  # Difference from long-term mean in SE units
  se_diff <- (w_mean - overall_mean) / pooled_se
  
  return(data.frame(mean_TP = w_mean,
                    pooled_SE = pooled_se,
                    SEs_from_overall = se_diff))
}

# Step 3: Subset your data by time period
p1 <- BB.TP %>% filter(Year >= 1965 & Year <= 1976)
p2 <- BB.TP %>% filter(Year >= 1977 & Year <= 2007)
p3 <- BB.TP %>% filter(Year >= 2008 & Year <= 2022)

# Step 4: Apply the function to each period
p1_stats <- period_stats(p1)
p2_stats <- period_stats(p2)
p3_stats <- period_stats(p3)

# Step 5: Combine results into one summary table
period_summary <- bind_rows(
  cbind(Period = "1965–1976", p1_stats),
  cbind(Period = "1977–2007", p2_stats),
  cbind(Period = "2008–2022", p3_stats)
)

# View results
print(period_summary)




