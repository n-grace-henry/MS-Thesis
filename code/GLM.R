setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)

#Load Data
data <- read.csv(file = "final/data.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")
NPGO <- read.csv(file = "Environmental/NPGO.csv")

#PDO average per year
PDO_annual <- matrix(nrow = length(PDO$Year), ncol = 2)
for(i in 1:length(PDO$Year)){
  PDO_annual[i,1] <- PDO$Year[i]
  PDO_annual[i,2] <- mean(as.numeric(PDO[i, 2:13]))
}
plot(PDO_annual,
     type = "l")
ts.PDO <- ts(PDO_annual[,2], 
             start = PDO_annual[1,1],
             end = PDO_annual[169,1],
             frequency = 1)
plot(x = ts.PDO)
cpt <- cpt.mean(as.vector(scale(ts.PDO)), method = "PELT")
plot(cpt, cpt.col = "blue")
summary(cpt)

#NPGO average per year
NPGO_annual <- NPGO %>%
  group_by(Year) %>%
  summarise(avg = mean(NPGO))
plot(NPGO_annual, type = "l")
ts.NPGO <- ts(NPGO_annual[,2],
              start = NPGO_annual[1,1],
              end = NPGO_annual[72,1],
              frequency = 1)
cpt.N <- cpt.mean(as.vector(scale(ts.NPGO)), method = "PELT")
plot(cpt.N, cpt.col ="blue")

#GLM



#Steps given by Chat GPT, read through and adjust as necessary 
# Step 1: Load Data
# Assuming your dataset is loaded into a dataframe called 'data'
# Replace 'data.csv' with your actual file path if you're loading from a CSV file
data <- read.csv("data.csv")

# Step 2: Explore Data
summary(data)
str(data)
# Visualize relationships using plots, e.g., scatterplot between isotope signature and PDO

# Step 3: Fit GLM
# Assuming 'isotope_signature' is your response variable and 'PDO' is your predictor variable
glm_model <- glm(isotope_signature ~ PDO, data = data, family = gaussian)

# Step 4: Assess Model Fit
summary(glm_model)
# Check residuals, residual plots, goodness-of-fit measures, etc.

# Step 5: Interpret Results
# Extract coefficients
coefficients(glm_model)
# Interpret coefficients and assess significance


