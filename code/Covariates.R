# Load packages 
library(tidyverse)
library(MARSS)
library(ggplot2)

# Load PHE data
PHE <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_PHE_formatted.csv")
PHE <- PHE[,-1]

# Load GLU data
GLU <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_GLU_formatted.csv")
GLU <- GLU[,-1]

# Load environmental data 
PDO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")
ENSO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/ENSO_tidy.csv")
NPGO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO_tidy.csv")
ret <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/BB_returns.csv")
ice <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/sea_ice.csv")
# SST <- 

# Put all envi data into one df (only two regimes until i figure this out)
climate <- merge(PDO, NPGO, by = "Year")
climate <- climate[,-c(2,4)]
colnames(climate) <- c("Year", "PDO", "NPGO")

# Transpose climate 
climate <- t(climate)
climate <- climate[-1,]

# Model environmental covariates vs states 
# Z-score the response variables (phe) 
the.mean <- apply(PHE, 1, mean, na.rm = TRUE)
the.sigma <- sqrt(apply(PHE, 1, var, na.rm = TRUE))
PHE <- (PHE - the.mean) * (1/the.sigma)

# z-score the covariates
the.mean <- apply(climate, 1, mean, na.rm = TRUE)
the.sigma <- sqrt(apply(climate, 1, var, na.rm = TRUE))
covariates <- (climate - the.mean) * (1/the.sigma)

# Define model parameters
ZZ <- matrix(1, 9, 1)

# both process and observation error, but covariates only affect the process
D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "diagonal and unequal"
Q <- "equalvarcov"
C <- "unconstrained"
c <- covariates
R <- diag(0.16, 2)
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)
autoplot(kem)





# From chatGPT 
# Assuming PDO is a vector with 58 values corresponding to each year from 1965-2022
PDO <- as.vector(PDO_data)  # replace PDO_data with your actual PDO data vector

# Create a matrix where each row corresponds to a river system and each column to a year
# Each system experiences the same PDO each year, so replicate PDO across rows
covariate_matrix <- matrix(PDO, nrow = 18, ncol = 58, byrow = TRUE)




# Define the C matrix for the covariate
C_matrix <- matrix(0, 3, 1)  # Assuming PDO has one influence parameter per state
C_matrix[1, 1] <- "c1"       # Influence on state 1
C_matrix[2, 1] <- "c2"       # Influence on state 2
C_matrix[3, 1] <- "c3"       # Influence on state 3

# Update the model list to include the covariate
mod.list <- list(
  B = "identity",  
  U = "zero",          
  Q = "diagonal and equal",         
  Z = ZZ,
  A = "scaling",    
  R = "diagonal and unequal",
  x0 = matrix(c("mu1", "mu2", "mu3"), nrow = 3, ncol = 1),      
  tinitx = 0,
  C = C_matrix,
  c = covariate_matrix
)

# Fit the model with the environmental covariate PDO
fit.phe <- MARSS(all.PHE, model = mod.list)






