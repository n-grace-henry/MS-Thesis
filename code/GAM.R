# Load packages
library(dplyr)
library(mgcv)
library(readr)

# Read in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

# Fit GAM of Trophic Position 
gam_model_trophic <- gam(Trophic.Position ~ s(Year), data = data, method = "REML")
summary(gam_model_trophic) #shows that this used about 3.034 degrees of freedom
# null hypothesis is that the function is a flat line, we reject this null with p = 0.00217
# this GAM explains 19.7% of the deviance in the data

# Plot GAM
plot(gam_model)
plot(x = data$Year,
     y = data$Trophic.Position)

# Plot GAM of Phenylalanine d15N 
gam_model_phe <- gam(PHE.mean ~ s(Year), data = data)
summary(gam_model_phe)

# Notes from online lecture 
# Generalized Additive Models (GAMs) 
# 


