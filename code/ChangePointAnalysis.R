setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv(file = "data.csv")

#load packages
library(changepoint)

#visualize
library(ggplot2)
ggplot(data, aes(x = Year,
                 y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7)

# Getting the data in a format that will work to create a time series object
# Convert the "Year" column to a factor to ensure correct ordering
data$Year <- factor(data$Year, levels = rev(unique(data$Year)))

# Change the years that were missing so that the data frame years are consistent
data[c(61,62), 2] = "1992" 
data[c(79,80), 2] = "1983"
data[c(113,114), 2] = "1968" 

# Make new data frames for each system 
Wood.all <- data[data$System == "Wood",]
Kvi.all <- data[data$System == "Kvichak",]
Eg.all <- data[data$System == "Egegik",]

# Plotting
plot(x = as.numeric(Wood.all[Wood.all$Age == "2", "Year"]),
     y = Wood.all[Wood.all$Age == "2", "PHE.mean"],
     type = "l")

plot(x = as.numeric(Kvi.all[Kvi.all$Age == "2", "Year"]),
     y = Kvi.all[Kvi.all$Age == "2", "PHE.mean"],
     type = "l")

plot(x = as.numeric(Eg.all[Eg.all$Age == "2", "Year"]),
     y = Eg.all[Eg.all$Age == "2", "PHE.mean"],
     type = "l")

#### Analysis Using Full Data Set ####

# Average PHE across all ages and systems (should have one data point per year)
library(dplyr)
avg_data_all <- data %>%
  group_by(Year) %>%
  summarise(avg_PHE = mean(PHE.mean, na.rm = TRUE))

print(avg_data_all)
plot(avg_data_all)

# Convert data to a time series object
ts_data <- ts(avg_data_all$avg_PHE, 
              start = avg_data_all$Year[1],
              end = avg_data_all$Year[length(avg_data_all$Year)], 
              frequency = 1)
plot(ts_data)

# Scale the data so the variance is 1 and mean is 0 and do CP analysis
ts_1_scale <- cpt.mean(as.vector(scale(ts_data)), method = "PELT")

# Plot the change point analysis results
plot(ts_1_scale, cpt.col = "blue")
summary(ts_1_scale)

# Average PHE across all systems with only age 2
age.2 <- data[data$Age == "2",]

library(dplyr)
avg_data_2 <- age.2 %>%
  group_by(Year) %>%
  summarise(PHE.mean = mean(PHE.mean, na.rm = TRUE))

# Convert data to a time series object
ts_data_2 <- ts(avg_data_2$PHE.mean, 
              start = avg_data_2$Year[1],
              end = avg_data_2$Year[length(avg_data_2$Year)], 
              frequency = 1)
plot(ts_data_2)

# Scale the data so the variance is 1 and mean is 0 and then do CP analysis
ts_2_scale <- cpt.mean(as.vector(scale(ts_data_2)))

# Plot the change point analysis results
plot(ts_2_scale, cpt.col = "blue")
summary(ts_2_scale)


# Average PHE for Wood with both age classes
library(dplyr)
Wood <- Wood.all %>%
  group_by(Year) %>%
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

# Convert data to a time series object
ts_Wood <- ts(Wood$PHE, 
                start = Wood$Year[1],
                end = Wood$Year[length(Wood$Year)], 
                frequency = 1)
plot(ts_Wood)

# Perform change point analysis using the 'cpt.mean' function
cpt.Wood <- cpt.mean(as.vector(scale(ts_Wood)))

# Plot the change point analysis results
plot(cpt.Wood, cpt.col = "blue")
summary(cpt_result)


# Average PHE for Kvi with both age classes
Kvichak <- Kvi.all %>%
  group_by(Year) %>%
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

# Convert data to a time series object
ts_Kvichak <- ts(Kvichak$PHE, 
              start = Kvichak$Year[1],
              end = Kvichak$Year[length(Kvichak$Year)], 
              frequency = 1)
plot(ts_Kvichak)

# Perform change point analysis using the 'cpt.mean' function
cpt.Egegik <- cpt.mean(as.vector(scale(ts_Egegik)))

# Plot the change point analysis results
plot(cpt.Kvichak, cpt.col = "blue")
summary(cpt.Egegik)


# Average PHE for Egegik with both age classes
Egegik <- Eg.all %>%
  group_by(Year) %>%
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

# Convert data to a time series object
ts_Egegik <- ts(Egegik$PHE, 
                 start = Egegik$Year[1],
                 end = Egegik$Year[length(Egegik$Year)], 
                 frequency = 1)
plot(ts_Egegik)

# Perform change point analysis using the 'cpt.mean' function
cpt.Egegik <- cpt.mean(as.vector(scale(ts_Egegik)))

# Plot the change point analysis results
plot(cpt.Egegik, cpt.col = "blue")
summary(cpt.Egegik)

