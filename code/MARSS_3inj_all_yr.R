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

# Model across all years 
mod.list.1 <- list(
  B = matrix(1),           # State transition matrix
  U = matrix(0),           # No deterministic trend
  Q = matrix("q"),         # Process noise covariance
  Z = matrix(1, 3, 1),     # Observation matrix with 3 observations per time point
  A = matrix(0, 3, 1),     # No observation bias, correct dimensions
  R = "diagonal and unequal",# Observation noise structure (diagonal and equal)
  x0 = matrix("mu"),       # Initial state estimate
  tinitx = 0               # Initial time point
)

# Fitting the model
fit.1 <- MARSS(wide.t, model = mod.list.1)
autoplot(fit.1)


