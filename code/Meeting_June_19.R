setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

# Load packages
library(tidyverse)
library(mgcv)
library(MARSS)

# Load data
data <- read.csv(file = "trophic_position.csv")


#### Paired method ####
# Plot PHE
ggplot(data, aes(Year, PHE.mean)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Paired Method", x = "Year", y = "PHE")

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





