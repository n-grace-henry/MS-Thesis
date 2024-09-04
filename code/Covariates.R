# Notes from ATSA class
# Dd implies that observations are impacted by covariates instead of process
# We should probably only put covariates in the process equation (Cc)

# Process error only model from textbook 
R <- A <- U <- "zero"
B <- Z <- "identity"
Q <- "equalvarcov"
C <- "unconstrained"
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   C = C, c = covariates)
kem <- MARSS(dat, model = model.list)


# Load packages 
library(tidyverse)
library(MARSS)
library(ggplot2)

# Load PHE data
PHE <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_PHE_formatted.csv")
PHE <- as.matrix(PHE[,-1])

# Load GLU data
GLU <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_GLU_formatted.csv")
GLU <- as.matrix(GLU[,-1])

# Load environmental data 
PDO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")
ENSO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/ENSO_tidy.csv")
NPGO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO_tidy.csv")
ret <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/BB_returns.csv")
ice <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/sea_ice.csv")
# SST <- 

# Put all envi data into one df 
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

# Define model parameters for one state model 
ZZ <- matrix(1, 18, 1)

# both process and observation error, but covariates only affect the process
model.list <- list(B = "diagonal and unequal", 
                   U = "zero",
                   Q = "equalvarcov", 
                   Z = ZZ, 
                   A = "zero", 
                   R = "diagonal and unequal", 
                   D = "zero", 
                   d = "zero", 
                   C = "unconstrained", 
                   c = climate, 
                   x0 = "unequal", 
                   tinitx = 1)
fit.one <- MARSS(PHE, model = model.list)
autoplot(fit.one)

# Define model parameters for three state model 
ZZ <- matrix(0, 18, 3) 
ZZ[1:6, 1] <- 1
ZZ[7:12, 2] <- 1
ZZ[13:18, 3] <- 1

# PHE model specifications
model.list <- list(B = "diagonal and unequal", 
                   U = "zero",
                   Q = "equalvarcov", 
                   Z = ZZ, 
                   A = "zero", 
                   R = "diagonal and unequal", 
                   D = "zero", 
                   d = "zero", 
                   C = "unconstrained", 
                   c = climate, 
                   x0 = matrix(c("mu1", "mu2", "mu3"), nrow = 3, ncol = 1), 
                   tinitx = 1)
fit.three <- MARSS(PHE, model = model.list)
autoplot(fit.three)





