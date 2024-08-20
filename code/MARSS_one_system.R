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
plot(x = PHE.W$Year, y = PHE.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Subset Kvichak PHE 
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.K$Year, y = PHE.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Subset Egegik PHE
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.E$Year, y = PHE.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Subset data for age 2 and only PHE
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "System", "Age", "adj", "ID1", "Rep")]

# Subset Wood GLU
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.W$Year, y = GLU.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU", main = "GLU Wood")

# Subset Kvichak GLU
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.K$Year, y = GLU.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU", main = "GLU Kvichak")

# Subset Egegik GLU
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.E$Year, y = GLU.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU", main = "GLU Egegik")

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



