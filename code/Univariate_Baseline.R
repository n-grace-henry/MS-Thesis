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
merged_df <- merged_df[order(merged_df$Year, merged_df$Age, merged_df$System), ]

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

# Plot time series objects (age 2)
plot(Wood2_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
plot(Egegik2_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
plot(Kvichak2_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")

# Plot time series objects (age 3)
plot(Wood3_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
plot(Egegik3_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
plot(Kvichak3_ts, type = "o", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")

### Overall system trends combined not averaged ####
# Wood
Wood.all <- merged_df[merged_df$System == "Wood", "PHE.mean"]
Wood.all.ts <- ts(Wood.all, start = 1965, frequency = 2)

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

fit.W.all <- MARSS(Wood.all.ts, model = mod.list)
years <- seq(from = 1965, to = 2022.5, by = 0.5)

plot(Wood.all.ts, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(years, fit.W.all$states[1,], col = "red")

# Egegik 
Egegik.all <- merged_df[merged_df$System == "Egegik", "PHE.mean"]
Egegik.all.ts <- ts(Egegik.all, start = 1965, frequency = 2)

fit.E.all <- MARSS(Egegik.all.ts, model = mod.list)
plot(Egegik.all.ts, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(years, fit.E.all$states[1,], col = "red")

# Kvichak 
Kvichak.all <- merged_df[merged_df$System == "Kvichak", "PHE.mean"]
Kvichak.all.ts <- ts(Kvichak.all, start = 1965, frequency = 2)

fit.K.all <- MARSS(Kvichak.all.ts, model = mod.list)
plot(Kvichak.all.ts, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(years, fit.K.all$states[1,], col = "red")

#### Glutamic acid for age 2 ####

# Wood
Wood.glu <- merged_df[merged_df$System == "Wood" & merged_df$Age == 2, "GLU.mean"]
Wood.glu.ts <- ts(Wood.glu, start = 1965, frequency = 1)

fit.W.glu <- MARSS(Wood.glu.ts, model = mod.list)
plot(Wood.glu.ts, 
     type = "p", 
     col = "blue", 
     xlab = "Year", 
     ylab = "GLU.mean", 
     main = "Time Series Plot")
lines(1965:2022, fit.W.glu$states[1,], col = "red")

# Egegik 
Egegik.glu <- merged_df[merged_df$System == "Egegik" & merged_df$Age == 2, "GLU.mean"]
Egegik.glu.ts <- ts(Egegik.glu, start = 1965, frequency = 1)

fit.E.glu <- MARSS(Egegik.glu.ts, model = mod.list)
plot(Egegik.glu.ts, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "Time Series Plot")
lines(1965:2022, fit.E.glu$states[1,], col = "red")

# Kvichak 
Kvichak.glu <- merged_df[merged_df$System == "Kvichak" & merged_df$Age == 2, "GLU.mean"]
Kvichak.glu.ts <- ts(Kvichak.glu, start = 1965, frequency = 1)

fit.K.glu <- MARSS(Kvichak.glu.ts, model = mod.list)
plot(Kvichak.glu.ts, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "Time Series Plot")
lines(1965:2022, fit.K.glu$states[1,], col = "red")

#### Model Univariate State-Space Baseline Age 2 only ####

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
fit.W <- MARSS(Wood2_ts, model = mod.list)
plot(Wood2_ts, 
     type = "o", 
     col = "blue", 
     xlab = "Year", 
     ylab = "Phenylalanine 15N/14N", 
     main = "Wood - Age 2")
lines(1965:2022, fit.W$states[1,], col = "red")

# Kvichak 
fit.K <- MARSS(Kvichak2_ts, model = mod.list)
plot(Kvichak2_ts, 
     type = "o", 
     col = "blue", 
     xlab = "Year", 
     ylab = "Phenylalanine 15N/14N", 
     main = "Kvichak - Age 2")
lines(1965:2022, fit.K$states[1,], col = "red")

# Egegik 
fit.E <- MARSS(Egegik2_ts, model = mod.list)
plot(Egegik2_ts, 
     type = "o", 
     col = "blue", 
     xlab = "Year", 
     ylab = "Phenylalanine 15N/14N", 
     main = "Egegik - Age 2")
lines(1965:2022, fit.E$states[1,], col = "red")


#### Trophic Position from both modeled baselines ####

# State vectors 
W.phe <- fit.W$states[1,]
E.phe <- fit.E$states[1,]
K.phe <- fit.K$states[1,]

W.glu <- fit.W.glu$states[1,]
E.glu <- fit.E.glu$states[1,]
K.glu <- fit.K.glu$states[1,]

# Function to calculate trophic position from two vectors 
tp <- function(phe, glu){
  beta <- 3.4
  TDF <- 7.06
  
  tp <- vector(length = length(phe))
  for(i in 1:length(phe)){
    tp[i] <- 1 + ((glu[i]-phe[i]-beta)/TDF)
  }
  
  print(tp)
}

# Calculate trophic position
W.tp <- tp(W.phe, W.glu)
E.tp <- tp(E.phe, E.glu)
K.tp <- tp(K.phe, K.glu)

# Plot trophic position
plot(W.tp, type = "l", col = "blue", xlab = "Year", ylab = "Trophic Position", main = "Wood TP ")
plot(E.tp, type = "l", col = "blue", xlab = "Year", ylab = "Trophic Position", main = "Egegik TP")
plot(K.tp, type = "l", col = "blue", xlab = "Year", ylab = "Trophic Position", main = "Kvichak TP")


