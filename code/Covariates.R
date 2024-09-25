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

# Put all envi data into one df 
climate <- cbind(PDO, ENSO, NPGO)
climate <- climate[,c(2,3,6,9)]
colnames(climate) <- c("Year", "PDO", "ENSO", "NPGO")

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
                   Q = "equalvarcov", #related to each other, if one goes up the other might also go up, try unconstrained maybe 
                   Z = ZZ, 
                   A = "zero", 
                   R = "diagonal and equal", #consistent sampling 
                   D = "zero", 
                   d = "zero", 
                   C = "unconstrained", #maybe diagonal and equal if all systems are equally impacted by climate
                   c = climate, 
                   x0 = "unequal", 
                   tinitx = 1)
fit.one <- MARSS(PHE, model = model.list, method = "BFGS")
autoplot(fit.one)

#check colinearity between PDO and ENSO 
#can compare coefficients because standardized 
fit.one$coef[22]
fit.one$coef[23]
fit.one$coef[24]

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

# Terrance for loop code
Q_vec = c("diagonal and equal", "diagonal and unequal", "equalvarcovar")
Z_vec = c("big boy", "3 rivers")
Covariate_vec = c("PDO", "ENSO", "PDO+ENSO")
big_dateframe = NULL
for (Q_i in Q_vec){
  marrs_list$Q = Q_i 
  for (Z_i in Z_vec){
    if (Z_i == "big boy"){marrs_list = Z1} else {marrs_list = Z2} ## you will have to define the matrix outside loop
    for (Covariate_i in Covariate_vec){
      if(Covariate_i == "PDO") {data_input_matrix = C_pdo} ## you will have to define the matrix outside loop
      if(Covariate_i == "ENSO") {data_input_matrix = C_enso} ## you will have to define the matrix outside loop
      if(Covariate_i == "PDO+ENSO") {data_input_matrix = C_enso_pdo} ## you will have to define the matrix outside loop
      
      ## run the model with the updated model specifications and covariate data
      fit = MARSS(marss_list, method = "BFGS")
      
      # save stuff out
      AIC_i = fit$AICc; warning_i = fit$warnings;
      df_tmp  = data.frame(Q = Q_i, Z = Z_i, Covariates = Covarite_i, AIC = AIC_i, warning=warning_i)
      big_dateframe = rbind(big_dateframe, df_tmp)
    }}}





