library(RTMB)
library(gtools)
library(tidyverse)
library(ggplot2)
set.seed(1345)

yrs <- 100
log_total_resid_sd <- log(10)
logit_p_proc_var <- -3
total_resid_sd <- exp(log_total_resid_sd)
p_proc_var <- exp(logit_p_proc_var) / (1 + exp(logit_p_proc_var))
sigma_proc <- sqrt(p_proc_var * total_resid_sd^2)
sigma_obs <- sqrt((1 - p_proc_var) * total_resid_sd^2)

x1 <- 10
zets <- rnorm(yrs - 1, 0, sigma_proc)
x <- rep(NA, yrs)
x[1] <- x1
x[2:yrs] <- x1 + cumsum(zets) * sigma_proc
y <- rnorm(yrs, x, sigma_obs)

x1_2 <- 10
zets2 <- rnorm(yrs - 1, 0, sigma_proc)
x2 <- rep(NA, yrs)
x2[1] <- x1_2
x2[2:yrs] <- x1_2 + cumsum(zets2) * sigma_proc

x3 <- x + x2
y3 <- rnorm(yrs, x3, sigma_obs)

f <- function(parameters) {
  # Extract parameters
  x1 <- parameters$x1
  x1_2 <- parameters$x1_2
  zets <- parameters$zets
  zets2 <- parameters$zets2
  log_total_resid_sd <- parameters$log_total_resid_sd
  logit_p_proc_var <- parameters$logit_p_proc_var
  
  # Calculate derived quantities
  total_resid_sd <- exp(log_total_resid_sd)
  p_proc_var <- exp(logit_p_proc_var) / (1 + exp(logit_p_proc_var))
  sigma_proc <- sqrt(p_proc_var * total_resid_sd^2)
  sigma_obs <- sqrt((1 - p_proc_var) * total_resid_sd^2)
  
  # Initialize state variables
  yrs <- length(y)
  x <- rep(0, yrs)
  x2 <- rep(0, yrs)
  x[1] <- x1
  x2[1] <- x1_2
  
  # Compute state variables
  for (i in 2:yrs) {
    x[i] <- x[i - 1] + zets[i - 1] * sigma_proc
    x2[i] <- x2[i - 1] + zets2[i - 1] * sigma_proc
  }
  
  # Compute combined state variable
  x3 <- x + x2
  
  # Initialize negative log-likelihood
  nll <- 0
  
  # Random Effects Likelihood
  for (i in 1:(yrs - 1)) {
    nll <- nll - dnorm(zets[i], 0, 1, log = TRUE)
    nll <- nll - dnorm(zets2[i], 0, 1, log = TRUE)
  }
  
  # Observation Likelihood
  for (i in 1:yrs) {
    nll <- nll - dnorm(y[i] - x[i], 0, sigma_obs, log = TRUE)
    nll <- nll - dnorm(y3[i] - x3[i], 0, sigma_obs, log = TRUE)
  }
  
  # Report values
  REPORT(x)
  REPORT(x2)
  REPORT(x3)
  REPORT(sigma_proc)
  REPORT(sigma_obs)
  
  return(nll)
}


simdat<-list(y=y
             #,sd_p_proc = 0.5
             )

parameters<-list(
  x1 = 5,
  x1_2 = 5,
  zets = rnorm(length(simdat$y)-1,0,1),
  zets2 = rnorm(length(simdat$y)-1,0,1),
  log_total_resid_sd = log(2),
  logit_p_proc_var = 0
)



obj <- RTMB::MakeADFun(f, parameters,random = c("zets","zets2"))
opt <- nlminb(obj$par, obj$fn, obj$gr)
rep <- obj$report()
print(opt$par)
print(rep)
#sdreport(obj)


library(MARSS)
# State variables
X1 <- x
X2 <- x2
X3 <- x3
# Observations
Y1 <- y
Y3 <- y3
# Data for MARSS
dat <- rbind(Y1, Y3)
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
Q <- "diagonal and equal"
# R matrix (observation covariance matrix)
R <- "diagonal and equal"
# Initial states and covariance
x0 <- matrix(c(X1=5, X2=5,x3=10), nrow=3)
V0 <- diag(3)

# MARSS model list
mod_list <- list(Z=Z, B=B, U=U, Q=Q, R=R, x0=x0, V0=V0, tinitx=1)

# Fit the MARSS model
fit <- MARSS(dat, model=mod_list)


plot(y,type="b")
lines(x,col="blue")
lines(rep$x,col="red")
lines(t(fit$states)[,1],col="purple")


plot(y3,type="b")
lines(x3,col="blue")
lines(rep$x3,col="red")
lines(t(fit$states)[,3],col="purple")


plot(x2,type="l",col="blue")
lines(rep$x2,col="red")
lines(t(fit$states)[,2],col="purple")


plot(rep$x3~x3)
abline(a=0,b=1)
plot(t(fit$states)[,3]~x3)
abline(a=0,b=1)
plot(t(fit$states)[,3]~rep$x3)
abline(a=0,b=1)


sigma_obs
sigma_proc
fit$par$R%>%sqrt()
fit$par$Q%>%sqrt()
rep$sigma_obs
rep$sigma_proc


sqrt(mean((x- rep$x)^2))
sqrt(mean((x- fit$states[1,])^2))



sqrt(mean((x3- rep$x3)^2))
sqrt(mean((x3- fit$states[3,])^2))

