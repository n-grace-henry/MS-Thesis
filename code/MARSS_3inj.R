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

# Wood river data
# Format long and wide data frames for Wood system
PHE.W <- PHE[PHE$System == "Wood", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.W$Year, y = PHE.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Format data to transposed wide for 3 injections 
PHE.W.long <- PHE.W %>%
  arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID1, and Rep
  filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
  group_by(Year) %>%  # Group by Year
  mutate(SampleNumber = row_number()) %>%  # Assign unique sample numbers
  select(-ID1, -Rep)  # Remove columns ID1 and Rep

# Convert to wide format
PHE.wide <- PHE.W.long %>%
  pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")

# Transpose
PHE.wide.t <- t(PHE.wide)
PHE.wide.t <- PHE.wide.t[-1,]

# Run Wood PHE model 
mod.list.1 <- list(
  B = matrix(1),           # State transition matrix
  U = matrix(0),           # No deterministic trend
  Q = matrix("q"),         # Process noise covariance
  Z = matrix(1, 3, 1),     # Observation matrix with 3 observations per time point
  A = matrix(0, 3, 1),     # No observation bias, correct dimensions
  R = "diagonal and equal",# Observation noise structure (diagonal and equal)
  x0 = matrix("mu"),       # Initial state estimate
  tinitx = 0               # Initial time point
)

# Fitting the model
fit.1 <- MARSS(PHE.wide.t, model = mod.list.1)
autoplot(fit.1)

#### Write function to do model for each system ####
model <- function(data){
  
  # Format data to transposed wide for 3 injections 
  long <- data %>%
    arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID1, and Rep
    filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
    group_by(Year) %>%  # Group by Year
    mutate(SampleNumber = row_number()) %>%  # Assign unique sample numbers
    select(-ID1, -Rep)  # Remove columns ID1 and Rep
  
  # Convert to wide format
  wide <- long %>%
    pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")
  
  # Transpose
  wide.t <- t(wide)
  wide.t <- wide.t[-1,]
  
  # Run Wood PHE model 
  mod.list.1 <- list(
    B = matrix(1),           # State transition matrix
    U = matrix(0),           # No deterministic trend
    Q = matrix("q"),         # Process noise covariance
    Z = matrix(1, 3, 1),     # Observation matrix with 3 observations per time point
    A = matrix(0, 3, 1),     # No observation bias, correct dimensions
    R = "unconstrained",         # Observation noise structure (diagonal and equal)
    x0 = matrix("mu"),       # Initial state estimate
    tinitx = 0               # Initial time point
  )
  
  # Fitting the model
  fit <- MARSS(wide.t, model = mod.list.1)
  plots <- autoplot(fit)
  
  # Print plots
  return(plots)
}

PHE.W.mod <- model(PHE.W)

#### Egegik PHE ####
# Egegik 
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.E$Year, y = PHE.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Egegik")
model(PHE.E)

#### Kvichak PHE ####
# Kvichak
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(PHE.K$Year, PHE.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Kvichak")
model(PHE.K)


#### GLU ####
# Subset data for age 2 and only GLU 
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "adj", "System", "Age", "ID1", "Rep")]

#### Wood GLU ####
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.W$Year, y = GLU.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "GLU Wood")
model(GLU.W)

#### Egegik ####
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.E$Year, y = GLU.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "GLU Egegik")
model(GLU.E)

#### Kvichak ####
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.K$Year, y = GLU.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "GLU Kvichak")
model(GLU.K)


#### Subset state for each system and AA for TP calc ####




