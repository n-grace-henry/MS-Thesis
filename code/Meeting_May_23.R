setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

# Load packages
library(tidyverse)
library(mgcv)

# Load data
data <- read.csv(file = "trophic_position.csv")

#define beta and TDF values
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

#### GAM Method ####
# Plot of PHE with smooth GAM 
ggplot(data, aes(x = Year, y = PHE.mean, col = System)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()

# Run GAM on PHE through time 
gam_model_phe <- gam(PHE.mean ~ s(Year), data = data) 
summary(gam_model_phe)

# Plot of TP with smooth GAM as PHE baseline
ggplot(data, aes(x = Year, y = TP.GAM.Method)) + 
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE) +
  theme_minimal() + 
  labs(title = "GAM Baseline Method", x = "Year", y = "Trophic Position")


#### Time Period Method ####
# Average PHE for three time periods (periods based on plot)
period1 <- data[data$Year >= 1965 & data$Year <= 1982,]
period2 <- data[data$Year >= 1983 & data$Year <= 2005,]
period3 <- data[data$Year >= 2006 & data$Year <= 2022,]

# average PHE for each period
avg_phe1 <- period1$PHE.mean %>% mean(na.rm = TRUE)
avg_phe2 <- period2$PHE.mean %>% mean(na.rm = TRUE)
avg_phe3 <- period3$PHE.mean %>% mean(na.rm = TRUE)

# Plot of PHE with averages for each time period 
ggplot(data = data, aes(x = Year, y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_segment(x = 1965, xend = 1982, y = avg_phe1, yend = avg_phe1, color = "red") +
  geom_segment(x = 1983, xend = 2005, y = avg_phe2, yend = avg_phe2, color = "blue") +
  geom_segment(x = 2006, xend = 2022, y = avg_phe3, yend = avg_phe3, color = "green") +
  labs(title = "Average Phenylalanine (PHE) Across Time Periods",
       x = "Year",
       y = "Average PHE") +
  theme_minimal()

# Plot of TP using average PHE baseline
ggplot(data, aes(x = Year, y = TP.Average.Method)) + 
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE) +
  theme_minimal() + 
  labs(title = "Time Period Method", x = "Year", y = "Trophic Position")


#### Raw Data Method ####
# Plot PHE
ggplot(data, aes(Year, PHE.mean)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Raw Data Method", x = "Year", y = "PHE")

# Plot GLU
ggplot(data, aes(Year, GLU.mean)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE) +
  theme_minimal() + 
  labs(title = "Raw Data Method", x = "Year", y = "GLU")

# Plot of TP using raw data pairs as baseline
ggplot(data, aes(Year, Trophic.Position)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE) + 
  theme_minimal() + 
  labs(title = "Raw Data Method", x = "Year", y = "Trophic Position")


#### Modeling both PHE and GLU to predict TP ####
# Calculating GLU GAM model 
gam_model_glu <- gam(GLU.mean ~ s(Year), data = data) 
summary(gam_model_glu)

newdata.phe <- data.frame(Year = data$Year) #predict PHE from GAM
newdata.phe$PHE.predicted <- predict(gam_model_phe, newdata = newdata.phe)
newdata.glu <- data.frame(Year = data$Year) #predict PHE from GAM 
newdata.glu$GLU.predicted <- predict(gam_model_glu, newdata = newdata.glu)

# Calculate TP based on modeled PHE and modeled GLU
TP.mod.met <- 1 + ((newdata.glu$GLU.predicted - newdata.phe$PHE.predicted - beta)/TDF)

# Add to data frame 
data$TP.MOD.Method <- TP.mod.met

# Plot
ggplot(data, aes(Year, TP.MOD.Method)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "TP Modeled Method", x = "Year", y = "Trophic Position")




