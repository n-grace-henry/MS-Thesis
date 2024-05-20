# Load packages
library(tidyr)
library(dplyr)
library(mgcv)
library(readr)
library(effects)

# Read in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

# Fit GAM of Trophic Position 
gam_model_trophic <- gam(Trophic.Position ~ s(Year), data = data, method = "REML")
summary(gam_model_trophic) #shows that this used about 3.034 degrees of freedom
# null hypothesis is that the function is a flat line, we reject this null with p = 0.00217
# this GAM explains 19.7% of the deviance in the data

# Plot GAM
plot(gam_model_trophic)
plot(x = data$Year,
     y = data$Trophic.Position)

ggplot(data, aes(x = Year, y = Trophic.Position, col = System)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()
  
# Fit GAM of Phenylalanine d15N 
gam_model_phe <- gam(PHE.mean ~ s(Year), data = data) #year,system
summary(gam_model_phe)

# Plot GAM of Phenylalanine d15N
plot(gam_model_phe)
plot(x = data$Year,
     y = data$PHE.mean)

ggplot(data, aes(x = Year, y = PHE.mean, col = System)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()

# Fit GAM of Glutamic Acid d15N
gam_model_glu <- gam(GLU.mean ~ s(Year), data = data)
summary(gam_model_glu)

# Plot GAM of Glutamic Acid d15N
plot(gam_model_glu)
plot(x = data$Year,
     y = data$GLU.mean)

ggplot(data, aes(x = Year, y = GLU.mean, col = System)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()

# Effect of system on Trophic Position
data$Year <- as.numeric(data$Year)
data$System <- as.factor(data$System) #change data structure

sys_gam_factor <- gam(Trophic.Position ~ s(Year) + System, data = data)
summary(sys_gam_factor) #assumes the effect of system is constant over time, different intercepts 

#sys_gam <- gam(Trophic.Position ~ s(Year, System), data = data)
#summary(sys_gam) this doesn't work

sys_gam_by <- gam(Trophic.Position ~ s(Year, by = System) + System, data = data)
summary(sys_gam_by)

# AIC to compare models
AIC(sys_gam_factor, sys_gam_by, gam_model_trophic)
