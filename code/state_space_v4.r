library(RTMB)
library(gtools)
library(tidyverse)
library(ggplot2)
#set.seed(1345)

yrs <- 100
log_total_resid_sd <- log(c(10, 5))
logit_p_proc_var <- c(-3, 1)
x1 <- c(10, 10)
logit_corr_proc <- -0.5
corr_obs <- 0

# Calculate derived quantities
total_resid_sd <- exp(log_total_resid_sd)
p_proc_var <- exp(logit_p_proc_var) / (1 + exp(logit_p_proc_var))
sigma_proc <- sqrt(p_proc_var * total_resid_sd^2)
sigma_obs <- sqrt((1 - p_proc_var) * total_resid_sd^2)
corr_proc <- exp(logit_corr_proc) / (1 + exp(logit_corr_proc))

# Covariance matrices
Sigma_obs <- diag(sigma_obs^2)
Sigma_obs[1, 2] <- sigma_obs[1] * sigma_obs[2] * corr_obs
Sigma_obs[2, 1] <- Sigma_obs[1, 2]

Sigma_proc <- diag(sigma_proc^2)
Sigma_proc[1, 2] <- sigma_proc[1] * sigma_proc[2] * corr_proc
Sigma_proc[2, 1] <- Sigma_proc[1, 2]

# Generate process noise using independent standard normal variates
z <- matrix(rnorm((yrs - 1) * 2), ncol = 2)

# Reparameterize process noise
L_proc <- chol(Sigma_proc)
ets <- z %*% t(L_proc)

# Initialize state variables
x <- matrix(NA, nrow = yrs, ncol = 3)
x[1, 1:2] <- x1
x[2:yrs, 1:2] <- x1 + apply(ets, 2, cumsum)

# Compute combined state variable
x[, 3] <- x[, 1] + x[, 2]

# Generate observations
y <- t(apply(x[, c(1, 3)], 1, function(row) MASS::mvrnorm(1, row, Sigma_obs)))
y1=y[,1]
y3 = y[,2]

dat=list(y1,y3,x=x)
saveRDS(dat,"dat.rds")

f <- function(parameters) {
  RTMB::getAll(simdat)
  
  # Extract parameters
  x1 <- parameters$x1
  z <- parameters$z
  log_total_resid_sd <- parameters$log_total_resid_sd
  logit_p_proc_var <- parameters$logit_p_proc_var
  logit_corr_proc <- parameters$logit_corr_proc
  
  # Calculate derived quantities
  total_resid_sd <- exp(log_total_resid_sd)
  p_proc_var <- exp(logit_p_proc_var) / (1 + exp(logit_p_proc_var))
  sigma_proc <- sqrt(p_proc_var * total_resid_sd^2)
  sigma_obs <- sqrt((1 - p_proc_var) * total_resid_sd^2)
  corr_proc <- exp(logit_corr_proc) / (1 + exp(logit_corr_proc))
  corr_obs <- 0
  
  # Covariance matrices
  Sigma_obs <- diag(sigma_obs^2)
  Sigma_obs[1, 2] <- sigma_obs[1] * sigma_obs[2] * corr_obs
  Sigma_obs[2, 1] <- Sigma_obs[1, 2]
  Sigma_proc <- diag(sigma_proc^2)
  Sigma_proc[1, 2] <- sigma_proc[1] * sigma_proc[2] * corr_proc
  Sigma_proc[2, 1] <- Sigma_proc[1, 2]
  
  #Cholesky factorization of Sigma_proc
  L_proc <-matrix(0,ncol=ncol(Sigma_proc),nrow=nrow(Sigma_proc))
  for (i in 1:nrow(Sigma_proc)) {
    for (j in 1:i) {
      if (i == j) {
        #Diagonal elements
        L_proc[i, j] <- sqrt(Sigma_proc[i, i] - sum(L_proc[i, 1:(j-1)]^2))
      } else {
        # Off-diagonal elements
        L_proc[i, j] <- (Sigma_proc[i, j] - sum(L_proc[i, 1:(j-1)] * L_proc[j, 1:(j-1)])) / L_proc[j, j]
      }
    }
  }
  # Reparameterize process noise
  ets <- z %*% L_proc
  
  # Initialize state variables
  yrs <- nrow(y)
  x <- matrix(0,nrow=yrs,ncol=3)
  x[1,1:2] <- x1
  
  # Compute state variables
  for (i in 2:yrs) {
    x[i,1:2] <- x[i - 1,1:2] + ets[i - 1,1:2] 
  }
  
  # Compute combined state variable
  x[,3] <- x[,1] + x[,2]
  
  # Initialize negative log-likelihood
  nll <- 0
  
  # Random Effects Likelihood
  nll <- nll - sum(dnorm(z, 0, 1, log = TRUE))

  # Observation Likelihood
  for (i in 1:yrs) {
    nll <- nll - dmvnorm(y[i,] - x[i,c(1,3)], 0, Sigma_obs, log = TRUE)
  }
  
  # Report values
  REPORT(x)
  REPORT(sigma_proc)
  REPORT(sigma_obs)
  
  return(nll)
}

dat<-readRDS("dat.rds")
y<-dat[[1]]
y3<-dat[[2]]
x=dat[[3]]
y=cbind(y1,y3)
simdat<-list(y=y
             #,sd_p_proc = 0.5
             )

parameters<-list(
  x1 = c(5,5),
  z = matrix(0,nrow=nrow(simdat$y)-1,ncol=2),
  log_total_resid_sd = rep(log(2),2),
  logit_p_proc_var = rep(0,2),
  logit_corr_proc = 0
)


obj <- RTMB::MakeADFun(f, parameters,random = c("z"))
opt <- nlminb(obj$par, obj$fn, obj$gr)
rep <- obj$report()
print(opt$par)
print(rep)
#sdreport(obj)



library(MARSS)
# Data for MARSS
dat <- t(y)
# Z matrix (observation matrix)
# Observing X1 and X3, not X2
Z <- matrix(c(1, 0, 0,
              0, 0, 1), nrow=2, byrow=TRUE)
# B matrix (state transition matrix)
# X1[t+1] = X1[t] + noise
# X2[t+1] = X2[t] + noise
# X3[t+1] = X1[t+1] + X2[t+1]
B <- matrix(c(1, 0, 0,
              0, 1, 0,
              1, 1, 0), nrow=3, byrow=TRUE)
# U matrix (control matrix)
U <- matrix(0, nrow=3, ncol=1)
# Q matrix (state covariance matrix)
Q <- "unconstrained"
# R matrix (observation covariance matrix)
R <- "diagonal and unequal"
# Initial states and covariance
x0 <- matrix(c(X1=5, X2=5,x3=10), nrow=3)
V0 <- diag(3)

# MARSS model list
mod_list <- list(Z=Z, B=B, U=U, Q=Q, R=R, x0=x0, V0=V0, tinitx=1)

# Fit the MARSS model
fit <- MARSS(dat, model=mod_list)


plot(y[,1],type="b")
lines(x[,1],col="blue")
lines(rep$x[,1],col="red")
lines(t(fit$states)[,1],col="purple")


plot(y[,2],type="b")
lines(x[,3],col="blue")
lines(rep$x[,3],col="red")
lines(t(fit$states)[,3],col="purple")


plot(x[,2],type="l",col="blue")
lines(rep$x[,2],col="red")
lines(t(fit$states)[,2],col="purple")


plot(rep$x[,3]~x[,3])
abline(a=0,b=1)
plot(t(fit$states)[,3]~x[,3])
abline(a=0,b=1)
plot(t(fit$states)[,3]~rep$x[,3])
abline(a=0,b=1)


sigma_obs
sigma_proc
fit$par$R%>%sqrt()
fit$par$Q%>%sqrt()
rep$sigma_obs
rep$sigma_proc


sqrt(mean((x[,1]- rep$x[,1])^2))
sqrt(mean((x[,1]- fit$states[1,])^2))



sqrt(mean((x[,3]- rep$x[,3])^2))
sqrt(mean((x[,3]- fit$states[3,])^2))

