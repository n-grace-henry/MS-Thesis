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

# Function to subset by system 
subset_data <- function(data, system, aa) {
  df <- data %>% 
    filter(AAID == aa & System == system & Age == "2") %>%
    dplyr::select(Year, adj, ID1, Rep)
  
  return(df)
}

# Subset systems 
PHE.W <- subset_data(data.full, "Wood", "PHE")
PHE.K <- subset_data(data.full, "Kvichak", "PHE")
PHE.E <- subset_data(data.full, "Egegik", "PHE")

GLU.W <- subset_data(data.full, "Wood", "GLU")
GLU.K <- subset_data(data.full, "Kvichak", "GLU")
GLU.E <- subset_data(data.full, "Egegik", "GLU")

# Function to get all data to wide format 
wide <- function(data){
  # Full years to represent in data
  full_years <- seq(1965, 2022, by = 1)
  
  # Format data: six injections per year, every year represented
  long <- data %>%
    arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID, and Rep
    group_by(Year) %>%  # Group by Year
    mutate(SampleNumber = as.character(row_number())) %>%  # Assign and convert unique sample numbers to characters
    dplyr::select(-ID1, -Rep) %>%  # Remove columns ID and Rep
    ungroup() %>%  # Ungroup to avoid issues with expand.grid
    right_join(
      expand.grid(
        Year = full_years,
        SampleNumber = c("1", "2", "3", "4", "5", "6")
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
ZZ <- matrix(1, 18, 1)

# Modify the model specifications
mod.list <- list(
  B = "identity",  
  U = "zero",          
  Q = "diagonal and equal",         
  Z = ZZ,
  A = "scaling",    
  R = "diagonal and equal",
  x0 = matrix("mu", nrow = 1, ncol = 1),  
  tinitx = 0             
)

# Fit PHE model 
PHE.fit.one <- MARSS(all.PHE, model = mod.list, method = "BFGS")

# Fit GLU model 
GLU.fit.one <- MARSS(all.GLU, model = mod.list, method = "BFGS")

# Subset states
PHE.state <- PHE.fit.one$states
GLU.state <- GLU.fit.one$states

# Subset SE states
PHE.states.SE <- PHE.fit.one$states.se
GLU.states.SE <- GLU.fit.one$states.se

# Trophic position calculations
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

tp <- (((GLU.state - PHE.state)-beta)/TDF) + 1

# Calculate standard error on TP 
tp.SE <- sqrt((GLU.states.SE^2 + PHE.states.SE^2)/TDF^2)

# Fit PHE system model 
system_ZZ <- matrix(0, 18, 3) 
system_ZZ[1:6, 1] <- 1
system_ZZ[7:12, 2] <- 1
system_ZZ[13:18, 3] <- 1

model.list <- list(B = "identity", 
                   U = "zero",
                   Q = "diagonal and equal", 
                   Z = system_ZZ, 
                   A = "scaling", 
                   R = "diagonal and equal",
                   x0 = matrix(c("mu1", "mu2", "mu3"), nrow = 3, ncol = 1), 
                   tinitx = 0)
PHE.system <- MARSS(all.PHE, model = model.list, method = "BFGS")

# Fit GLU system model
GlU.system <- MARSS(all.GLU, model = model.list, method = "BFGS")

# Subset PHE states
W.PHE <- PHE.system[["states"]][1,]
K.PHE <- PHE.system[["states"]][2,]
E.PHE <- PHE.system[["states"]][3,]

# Subset SE states
W.PHE.SE <- PHE.system[["states.se"]][1,]
K.PHE.SE <- PHE.system[["states.se"]][2,]
E.PHE.SE <- PHE.system[["states.se"]][3,]

# Subset GLU states
W.GLU <- GlU.system[["states"]][1,]
K.GLU <- GlU.system[["states"]][2,]
E.GLU <- GlU.system[["states"]][3,]

# Subset SE states
W.GLU.SE <- GlU.system[["states.se"]][1,]
K.GLU.SE <- GlU.system[["states.se"]][2,]
E.GLU.SE <- GlU.system[["states.se"]][3,]

# Calculate TP for each system 
tp.W <- (((W.GLU - W.PHE)-beta)/TDF) + 1
tp.K <- (((K.GLU - K.PHE)-beta)/TDF) + 1
tp.E <- (((E.GLU - E.PHE)-beta)/TDF) + 1

# Calculate standard error on TP for each system 
tp.W.SE <- sqrt((W.GLU.SE^2 + W.PHE.SE^2)/TDF^2)
tp.K.SE <- sqrt((K.GLU.SE^2 + K.PHE.SE^2)/TDF^2)
tp.E.SE <- sqrt((E.GLU.SE^2 + E.PHE.SE^2)/TDF^2)

# Make df of all states
PHE.state <- PHE.state[1,]
GLU.state <- GLU.state[1,]

all.states <- data.frame(
  W.PHE = as.vector(W.PHE),
  K.PHE = as.vector(K.PHE),
  E.PHE = as.vector(E.PHE),
  W.PHE.SE = as.vector(W.PHE.SE),
  K.PHE.SE = as.vector(K.PHE.SE),
  E.PHE.SE = as.vector(E.PHE.SE),
  W.GLU = as.vector(W.GLU),
  K.GLU = as.vector(K.GLU),
  E.GLU = as.vector(E.GLU),
  W.GLU.SE = as.vector(W.GLU.SE),
  K.GLU.SE = as.vector(K.GLU.SE),
  E.GLU.SE = as.vector(E.GLU.SE),
  PHE.state = as.vector(PHE.state),
  GLU.state = as.vector(GLU.state),
  tp.W = as.vector(tp.W),
  tp.K = as.vector(tp.K),
  tp.E = as.vector(tp.E),
  tp = as.vector(tp),
  PHE.states.SE = as.vector(PHE.states.SE),
  GLU.states.SE = as.vector(GLU.states.SE),
  tp.SE = as.vector(tp.SE),
  tp.W.SE = as.vector(tp.W.SE),
  tp.K.SE = as.vector(tp.K.SE),
  tp.E.SE = as.vector(tp.E.SE)
)

year <- 1965:2022
all.states$Year <- year

colnames(all.states) <- c("W.PHE", 
                          "K.PHE", 
                          "E.PHE", 
                          "W.PHE.SE",
                          "K.PHE.SE",
                          "E.PHE.SE",
                          "W.GLU", 
                          "K.GLU", 
                          "E.GLU", 
                          "W.GLU.SE",
                          "K.GLU.SE",
                          "E.GLU.SE",
                          "BB.PHE",
                          "BB.GLU", 
                          "tp.W", 
                          "tp.K", 
                          "tp.E", 
                          "BB.tp",
                          "PHE.SE", 
                          "GLU.SE", 
                          "tp.SE",
                          "tp.W.SE",
                          "tp.K.SE",
                          "tp.E.SE",
                          "Year")

# Save as csv
write.csv(all.states, file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")

