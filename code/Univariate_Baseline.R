# Load packages 
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(ggplot2)
library(dplyr)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

#### Create time series objects ####

# Create a complete sequence of years
years <- seq(1965, 2022, by = 1)

# Create a complete data frame for each river system
complete_df <- expand.grid(Year = years, System = unique(data$System), Age = unique(data$Age))

# Merge with the original data to insert NA values for missing data
merged_df <- left_join(complete_df, data, by = c("Year", "System", "Age"))

# Split the data by river system and age class
Wood.2 <- merged_df %>% filter(System == "Wood") %>% filter(Age == 2)
Wood.3 <- merged_df %>% filter(System == "Wood") %>% filter(Age == 3)
Egegik.2 <- merged_df %>% filter(System == "Egegik") %>% filter(Age == 2)
Egegik.3 <- merged_df %>% filter(System == "Egegik") %>% filter(Age == 3)
Kvichak.2 <- merged_df %>% filter(System == "Kvichak") %>% filter(Age == 2)
Kvichak.3 <- merged_df %>% filter(System == "Kvichak") %>% filter(Age == 3)

# Convert to time series objects
Wood2.data <- c(4.280390, NA, NA, 3.345011, NA, NA, 6.680199, NA, NA,
                              4.416843, NA, NA, 4.794477, NA, NA, 5.024736, NA, NA,
                              NA, 6.662367, NA, 3.473617, NA, NA, 2.553377, NA, NA,
                              6.817998, NA, NA, 3.996405, NA, NA, 6.824048, NA, NA,
                              3.137087, NA, NA, 3.388897, NA, NA, 3.326144, NA, NA,
                              4.558063, NA, NA, 4.453277, NA, NA, 3.815537, NA, NA,
                              5.437850, NA, NA, 4.780483)
Wood2_ts <- ts(Wood2.data, start = 1965, frequency = 1)

Wood3.data <- c(NA, NA, NA, 3.629854, NA, NA, 2.452854, NA, NA,
                              NA, NA, NA, 1.397096, NA, NA, NA, NA, NA,
                              NA, NA, NA, 6.431029, NA, NA, 3.989506, NA, NA,
                              3.343838, NA, NA, 4.535749, NA, NA, NA, NA, NA,
                              6.479993, NA, NA, 7.014444, NA, NA, NA, NA, NA,
                              NA, NA, NA, 4.380171, NA, NA, 5.011158, NA, NA,
                              NA, NA, NA, 4.213152)
Wood3_ts <- ts(Wood3.data, start = 1965, frequency = 1)

Egegik2.data <- c(2.516461, NA, 1.935622, NA, NA, NA, 4.193070, NA, NA,
                   3.978130, NA, NA, 5.100270, NA, NA, NA, NA, NA,
                   NA, NA, NA, 7.048355, NA, NA, 5.921672, NA, NA,
                   3.707803, NA, NA, 4.765402, NA, NA, 2.385568, NA, NA,
                   5.395242, NA, NA, 4.403601, NA, NA, 6.318120, NA, NA,
                   4.104897, NA, NA, 1.579084, NA, NA, 3.183578, NA, NA,
                   4.675207, NA, NA, 2.289712)
Egegik2_ts <- ts(Egegik2.data, start = 1965, frequency = 1)

Egegik3.data <- c(NA, NA, 3.900713, NA, NA, NA, 3.865586, NA, NA,
                   NA, NA, NA, 1.412124, NA, NA, NA, NA, NA,
                   NA, NA, NA, NA, NA, NA, 2.500479, NA, NA,
                   NA, NA, NA, 8.215039, NA, NA, 5.709907, NA, NA,
                   5.060625, NA, NA, 2.409714, NA, NA, 2.346770, NA, NA,
                   2.219991, NA, NA, 3.351748, NA, NA, NA, NA, NA,
                   NA, NA, NA, 1.484118)
Egegik3_ts <- ts(Egegik3.data, start = 1965, frequency = 1)

Kvichak2.data <- c(4.249013, NA, NA, 1.855945, NA, NA, 3.255309, NA, NA,
                    3.564758, NA, NA, 4.489917, NA, NA, 3.403307, NA, NA,
                    3.956233, NA, NA, NA, NA, NA, 5.375161, NA, NA,
                    NA, 6.450664, NA, 3.342840, NA, NA, 6.973176, NA, NA,
                    6.354919, NA, NA, 8.714515, NA, NA, 6.008137, NA, NA,
                    3.284504, NA, NA, 5.925795, NA, NA, 3.762834, NA, NA,
                    5.237393, NA, NA, 4.866149)
Kvichak2_ts <- ts(Kvichak2.data, start = 1965, frequency = 1)

Kvichak3.data <- c(NA, NA, NA, NA, NA, NA, 2.855826, NA, NA, 
                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                   NA, NA, NA, NA, NA, NA, NA, 6.289370, NA, 
                   NA, 3.579844, NA, NA, NA, NA, NA, NA, 
                   NA, NA, 1.136779, NA, NA, NA, NA, NA, 
                   4.523967, NA, NA, 3.672443)

 
Kvichak3_ts <- ts(Kvichak3.data, start = 1965, frequency = 1)

# Plot the time series object
plot(Wood2_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")

#### Average Age 2 and Age 3 per River System ####

# Wood averages
Wood <- cbind(Wood2.data, Wood3.data)
Wood.data <- apply(Wood, 1, mean, na.rm = TRUE)
Wood.data_ts <- ts(Wood.data, start = 1965, frequency = 1)

plot(Wood.data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")

# Egegik averages
Egegik <- cbind(Egegik2.data, Egegik3.data)
Egegik.data <- apply(Egegik, 1, mean, na.rm = TRUE)
Egegik.data_ts <- ts(Egegik.data, start = 1965, frequency = 1)

plot(Egegik.data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")

# Kvichak averages 
Kvichak <- cbind(Kvichak2.data, Kvichak3.data)
Kvichak.data <- apply(Kvichak, 1, mean, na.rm = TRUE)
Kvichak.data_ts <- ts(Kvichak.data, start = 1965, frequency = 1)

plot(Kvichak.data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")


#### Model Univariate State-Space Baseline ####

# Wood
mod.list <- list(
  U = matrix("u"),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"), 
  tinitx = 0
)
fit.W <- MARSS(Wood.data_ts, model = mod.list)
plot(Wood.data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(1965:2022, fit.W$states[1,], col = "red")

# Kvichak 
fit.K <- MARSS(Kvichak.data_ts, model = mod.list)
plot(Kvichak.data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(1965:2022, fit.K$states[1,], col = "red")

# Egegik 
fit.E <- MARSS(Egegik.data_ts, model = mod.list)
plot(Egegik.data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(1965:2022, fit.E$states[1,], col = "red")

# Average between all systems 
all_data <- cbind(Wood.data, Egegik.data, Kvichak.data)
all_data_ts <- ts(apply(all_data, 1, mean, na.rm = TRUE), start = 1965, frequency = 1)

fit.all <- MARSS(all_data_ts, model = mod.list)
plot(all_data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(1965:2022, fit.all$states[1,], col = "red")

# Using all data to model baseline  

# ChatGPT code 

merged_df <- merged_df[order(merged_df$Year, merged_df$System, merged_df$Age), ]
values <- merged_df$PHE.mean
all_data_ts <- ts(values, start = 1965, frequency = 6)
plot(values)
length(all_data_ts)
# Define the model list (example)

# Fit the MARSS model
fit.all <- MARSS(all_data_ts, model = mod.list)

# Plot the data and fitted values
plot(all_data_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(1965 + (0:((length(fit.all$states[1,]) - 1) / 6)), fit.all$states[1,], col = "red")

str(all_data_ts)

