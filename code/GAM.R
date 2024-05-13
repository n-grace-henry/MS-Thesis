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
plot(gam_model_trophic)
plot(x = data$Year,
     y = data$Trophic.Position)

ggplot(data, aes(x = Year, y = Trophic.Position)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()
  
# Fit GAM of Phenylalanine d15N 
gam_model_phe <- gam(PHE.mean ~ s(Year), data = data)
summary(gam_model_phe)

# Plot GAM of Phenylalanine d15N
plot(gam_model_phe)
plot(x = data$Year,
     y = data$PHE.mean)

ggplot(data, aes(x = Year, y = PHE.mean)) +
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

ggplot(data, aes(x = Year, y = GLU.mean)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), col = "red") +
  theme_minimal()
