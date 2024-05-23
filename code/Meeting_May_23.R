setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

# Load packages
library(tidyverse)
library(mgcv)

# Load data
data <- read.csv(file = "trophic_position.csv")

# Plot of PHE with smooth GAM 
ggplot(data, aes(x = Year, y = PHE.mean, col = System)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()

# Run GAM on PHE through time 
gam_model_phe <- gam(PHE.mean ~ s(Year), data = data) 
summary(gam_model_phe)

