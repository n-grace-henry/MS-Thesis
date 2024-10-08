# Citation for package to include 
citation("MARSS")

# Load packages 
library(tidyverse)
library(MARSS)
library(ggplot2)

# Load PHE data: order of systems - Wood, Kvichak, Egegik
PHE <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_PHE_formatted.csv")
PHE <- as.matrix(PHE[,-1])

# Load GLU data
GLU <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_GLU_formatted.csv")
GLU <- as.matrix(GLU[,-1])

# Load environmental data 
PDO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")
ENSO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/ENSO_tidy.csv")
NPGO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO_tidy.csv")
#ret <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/BB_returns.csv")
#ice <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/sea_ice.csv")

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

#check colinearity between PDO and ENSO 

# For loop to fit all versions of covariates model
BB_ZZ <- matrix(1, 18, 1)
system_ZZ <- matrix(0, 18, 3) 
system_ZZ[1:6, 1] <- 1
system_ZZ[7:12, 2] <- 1
system_ZZ[13:18, 3] <- 1

Q_vec <- c("diagonal and equal", "diagonal and unequal", "equalvarcov", "unconstrained")
C_vec <- c("diagonal and equal", "unconstrained")
Z_vec <- c("Bristol Bay", "Systems")
Covariate_vec <- c("PDO", "NPGO", "PDO + NPGO")
big_dateframe <- NULL
model.list <- list(B = "diagonal and unequal", 
                   U = "zero",
                   Q = "N/A", #will be updated in the loop
                   Z = "N/A", #will be updated in the loop
                   A = "zero", 
                   R = "diagonal and equal", #consistent sampling 
                   D = "zero", 
                   d = "zero", 
                   C = "unconstrained", #maybe diagonal and equal if all systems are equally impacted by climate
                   c = "N/A", #will be updated in the loop
                   x0 = "unequal", 
                   tinitx = 1)

for (Q_i in Q_vec){
  model.list$Q <- Q_i
  for(C_i in C_vec){
    model.list$C <- C_i
  for (Z_i in Z_vec){
    if (Z_i == "Bristol Bay"){model.list$Z <- BB_ZZ} else {model.list$Z <- system_ZZ}
    for (Covariate_i in Covariate_vec){
      if(Covariate_i == "PDO") {model.list$c <- matrix(covariates["PDO", ], nrow = 1)}
      if(Covariate_i == "NPGO") {model.list$c <- matrix(covariates["NPGO", ], nrow = 1)}
      if(Covariate_i == "PDO + NPGO") {model.list$c <- matrix(covariates[c("NPGO", "PDO"), ], nrow = 2)}
      
      ## run the model with the updated model specifications and covariate data
      fit <- tryCatch({
        MARSS(PHE, model = model.list, method = "BFGS")
      }, error = function(e) {
        NULL  # return NULL if the model fails
      })
      
      # save stuff out
      AIC_i <- if (!is.null(fit)) fit$AICc else NA
      df_tmp = data.frame(Q = Q_i, C = C_i, Z = Z_i, Covariates = Covariate_i, AIC = AIC_i)
      big_dateframe = rbind(big_dateframe, df_tmp)
    }}}}

# Run just best fit model 
model.list.1 <- list(B = "diagonal and unequal", 
                   U = "zero",
                   Q = "unconstrained", 
                   Z = system_ZZ,
                   A = "zero", 
                   R = "diagonal and equal",
                   D = "zero", 
                   d = "zero", 
                   C = "unconstrained", 
                   c = matrix(covariates["PDO", ], nrow = 1), 
                   x0 = "unequal", 
                   tinitx = 1)
output <- MARSS(PHE, model = model.list.1, method = "BFGS")
summary(output)
w <- output[["coef"]][["C.X1"]]
k <- output[["coef"]][["C.X2"]]
e <- output[["coef"]][["C.X3"]]
autoplot(output)

# Run just best fit model on BB
model.list.2 <- list(B = "diagonal and unequal", 
                     U = "zero",
                     Q = "unconstrained", 
                     Z = BB_ZZ,
                     A = "zero", 
                     R = "diagonal and equal",
                     D = "zero", 
                     d = "zero", 
                     C = "unconstrained", 
                     c = matrix(covariates["PDO", ], nrow = 1), 
                     x0 = "unequal", 
                     tinitx = 1)
output.2 <- MARSS(PHE, model = model.list.2, method = "BFGS")
summary(output.2)
bb <- output.2[["coef"]][["C.C"]]

# Trophic position estimation 
# Read in data
tp <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/tp.state.csv")
tp <- as.matrix(tp[,-1])
sys.tp <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/system.tp.states.csv")
sys.tp <- as.matrix(sys.tp[,-c(1,2)])
sys.tp <- t(sys.tp)

# Run model with TP instead of PHE 
ZZZ <- diag(3)
model.list.2 <- list(B = "diagonal and unequal", 
                     U = "zero",
                     Q = "unconstrained", 
                     Z = ZZZ,
                     A = "zero", 
                     R = "diagonal and equal",
                     D = "zero", 
                     d = "zero", 
                     C = "unconstrained", 
                     c = covariates, 
                     x0 = "unequal", 
                     tinitx = 1)
output.3 <- MARSS(sys.tp, model = model.list.2, method = "BFGS")
summary(output.3)



