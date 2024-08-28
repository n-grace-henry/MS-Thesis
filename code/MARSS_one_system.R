# Load packages 
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(zoo)
library(reshape2)
library(tidyverse)

# Load data 
data.full <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/full.csv")

# Subset data for age 2 and only PHE
PHE <- data.full[data.full$Age == "2" &
                   data.full$AAID == "PHE", c("Year", "System", "Age", "adj", "ID1", "Rep")]

# Subset Wood PHE
PHE.W <- PHE[PHE$System == "Wood", c("Year", "adj", "ID1", "Rep")]
# Subset Kvichak PHE 
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
# Subset Egegik PHE
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]

# Subset data for age 2 and only PHE
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "System", "Age", "adj", "ID1", "Rep")]

# Subset Wood GLU
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
# Subset Kvichak GLU
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
# Subset Egegik GLU
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]

# Function to get all data to wide format 
wide <- function(data){
  # Full years to represent in data
  full_years <- seq(1965, 2022, by = 1)
  
  # Format data: three injections per year, every year represented
  long <- data %>%
    arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID, and Rep
    filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
    group_by(Year) %>%  # Group by Year
    mutate(SampleNumber = as.character(row_number())) %>%  # Assign and convert unique sample numbers to characters
    select(-ID1, -Rep) %>%  # Remove columns ID and Rep
    ungroup() %>%  # Ungroup to avoid issues with expand.grid
    right_join(
      expand.grid(
        Year = full_years,
        SampleNumber = c("1", "2", "3")
      ),
      by = c("Year", "SampleNumber")
    ) %>%
    arrange(Year, SampleNumber)
  
  # Convert to wide format
  wide <- long %>%
    pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")
  
  # Transpose
  wide.t <- t(wide)
  wide.t <- wide.t[-1,]
  
  # Return data
  return(wide.t)
}

# Convert all PHE systems to wide using function
wide.PHE.W <- wide(PHE.W)
wide.PHE.K <- wide(PHE.K)
wide.PHE.E <- wide(PHE.E)

# Convert all GLU systmes to wide using function
wide.GLU.W <- wide(GLU.W)
wide.GLU.K <- wide(GLU.K)
wide.GLU.E <- wide(GLU.E)

# Combine all PHE systems
all.PHE <- rbind(wide.PHE.W, wide.PHE.K, wide.PHE.E)

# Combine all GLU systems
all.GLU <- rbind(wide.GLU.W, wide.GLU.K, wide.GLU.E)
 
# Adjust the Z matrix to assume one state for all systems
ZZ <- matrix(1, 9, 1)

# Modify the model specifications
mod.list <- list(
  B = "identity",  
  U = "zero",          
  Q = "diagonal and equal",         
  Z = ZZ,
  A = "scaling",    
  R = "diagonal and unequal",
  x0 = matrix("mu", nrow = 1, ncol = 1),  
  tinitx = 0             
)

# Fit PHE model 
PHE.fit.one <- MARSS(all.PHE, model = mod.list)
autoplot(PHE.fit.one)

# Calculate AICC of PHE.fit model 
AIC.one <- PHE.fit.one$AIC
k <- PHE.fit.one$num.params
n <- nrow(all.PHE) * ncol(all.PHE)
AICC.one <- AIC.one + (2 * k * (k + 1)) / (n - k - 1)

# Fit GLU model 
GLU.fit.one <- MARSS(all.GLU, model = mod.list)
autoplot(GLU.fit.one)

# Calculate AIC of GLU.fit model
AIC.GLU.one <- GLU.fit.one$AIC
k.GLU.one <- GLU.fit.one$num.params
n.GLU.one <- nrow(all.GLU) * ncol(all.GLU)
AICC.GLU.one <- AIC.GLU.one + (2 * k.GLU.one * (k.GLU.one + 1)) / (n.GLU.one - k.GLU.one - 1)

# Subset states
PHE.state <- PHE.fit.one$states
GLU.state <- GLU.fit.one$states

# Plot overall PHE state 
years <- seq(1965, 2022, by = 1)
plot(x = PHE$Year, y = PHE$adj, type = "p", pch = 16, col = "black", xlab = "Year", ylab = expression(paste("Phenylalanine ", delta^{15}, "N ")), main = "All Phenylalanine")
lines(x = years, y = PHE.state, type = "l", lwd = 2, col = "blue")

# Plot overall GLU state
plot(x = GLU$Year, y = GLU$adj, type = "p", pch = 16, col = "black", xlab = "Year", ylab = expression(paste("Glutamic Acid ", delta^{15}, "N")), main = "All Glutamic Acid")
lines(x = years, y = GLU.state, type = "l", lwd = 2, col = "red")

# Trophic position calculations
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

tp <- (((GLU.state - PHE.state)-beta)/TDF) + 1

# Plot TP 
plot(x = years, y = tp, type = "l", lwd =2, col = "black", xlab = "Year", ylab = "Trophic Position", main = "Overall Trophic Position")



