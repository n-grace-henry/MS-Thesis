# Load packages
library(dplyr)
library(mgcv)

# Read in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

# Fit GAM model
gam_model <- gam(Trophic.Position ~ s(Year), data = data)
summary(gam_model)

# Plot GAM
plot(gam_model)
plot(x = data$Year,
     y = data$Trophic.Position)


