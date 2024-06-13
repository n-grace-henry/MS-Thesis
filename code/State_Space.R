# Load packages
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(ggplot2)
library(colorspace) 
hcl_palette_52 <- colorspace::sequential_hcl(52)
hcl_oalette_100 <- colorspace::sequential_hcl(100)

# Univariate class example

# Model Nile data with 
# x_t = x_{t-1} + u + w_t, w_t ~ N(0, q)
# y_t = x_t + v_t, v_t ~ N(0, r)

mod.list <- list(
  U = matrix("u"),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"), 
  tinitx = 0
)
fit <- MARSS(Nile, model = mod.list)
plot(Nile)
lines(1871:1970, fit$states[1,], col = "red")
head(Nile)

# Run this model to generate a forecast
autoplot(forecast(fit, h = 10))

# Question 1
# Change the x (state) model to x_t = x_{t-1} + u 
# No error part, not a stochastic process
mod.list <- list(
  U = matrix("u"),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix(0),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"), 
  tinitx = 0
)
fit <- MARSS(Nile, model = mod.list)
autoplot(forecast(fit, h = 10))

# Question 2
# Change the x (state) model to x_t = x_{t-1} + w_t
# Got rid of u, so u = 0
# Expected value of forecast is flat
mod.list <- list(
  U = matrix(0),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"), 
  tinitx = 0
)
fit <- MARSS(Nile, model = mod.list)
autoplot(forecast(fit, h = 10))

# Question 3
# Change the x (state) model to x_t = u + w_t
# No longer a random walk, flat level with some error around it
mod.list <- list(
  U = matrix("u"),
  x0 = matrix(0),
  B = matrix(0),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"), 
  tinitx = 0
)
fit <- MARSS(Nile, model = mod.list)
autoplot(forecast(fit, h = 10))

#### Thomas code with notes ####

# Load libraries
library(RTMB)  # For Template Model Builder with R interface
library(gtools)  # General tools for data manipulation
library(tidyverse)  # Data manipulation and visualization
library(ggplot2)  # Data visualization

# Set a random seed for reproducibility
set.seed(1345)

# Define parameters for the simulation
yrs <- 100  # Number of years (time steps)
log_total_resid_sd <- log(10)  # Log of total residual standard deviation
logit_p_proc_var <- -3  # Logit of the process variance proportion

# Calculate the actual standard deviations from the parameters
total_resid_sd <- exp(log_total_resid_sd)  # Total residual standard deviation
p_proc_var <- exp(logit_p_proc_var) / (1 + exp(logit_p_proc_var))  # Process variance proportion
sigma_proc <- sqrt(p_proc_var * total_resid_sd^2)  # Process noise standard deviation
sigma_obs <- sqrt((1 - p_proc_var) * total_resid_sd^2)  # Observation noise standard deviation

# Simulate the first time series using a random walk
x1 <- 10  # Initial value for the first time series
zets <- rnorm(yrs - 1, 0, sigma_proc)  # Process noise for the first time series
x <- rep(NA, yrs)  # Initialize the time series
x[1] <- x1  # Set the first value
x[2:yrs] <- x1 + cumsum(zets) * sigma_proc  # Generate the random walk

# Simulate observations for the first time series
y <- rnorm(yrs, x, sigma_obs)  # Observations with added observation noise

# Simulate the second time series independently using another random walk
x1_2 <- 10  # Initial value for the second time series
zets2 <- rnorm(yrs - 1, 0, sigma_proc)  # Process noise for the second time series
x2 <- rep(NA, yrs)  # Initialize the second time series
x2[1] <- x1_2  # Set the first value
x2[2:yrs] <- x1_2 + cumsum(zets2) * sigma_proc  # Generate the random walk

# Create a combined state variable as the sum of the two time series
x3 <- x + x2  # Combined state variable
y3 <- rnorm(yrs, x3, sigma_obs)  # Observations for the combined state variable

# Define the negative log-likelihood function
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
  
  # Compute state variables using the process noise
  for (i in 2:yrs) {
    x[i] <- x[i - 1] + zets[i - 1] * sigma_proc
    x2[i] <- x2[i - 1] + zets2[i - 1] * sigma_proc
  }
  
  # Compute the combined state variable
  x3 <- x + x2
  
  # Initialize negative log-likelihood
  nll <- 0
  
  # Random Effects Likelihood: add the log-probabilities of process noise
  for (i in 1:(yrs - 1)) {
    nll <- nll - dnorm(zets[i], 0, 1, log = TRUE)
    nll <- nll - dnorm(zets2[i], 0, 1, log = TRUE)
  }
  
  # Observation Likelihood: add the log-probabilities of observations
  for (i in 1:yrs) {
    nll <- nll - dnorm(y[i] - x[i], 0, sigma_obs, log = TRUE)
    nll <- nll - dnorm(y3[i] - x3[i], 0, sigma_obs, log = TRUE)
  }
  
  # Report values for diagnostic purposes
  REPORT(x)
  REPORT(x2)
  REPORT(x3)
  REPORT(sigma_proc)
  REPORT(sigma_obs)
  
  return(nll)  # Return the negative log-likelihood
}

# Prepare the simulated data and initial parameter guesses
simdat <- list(y = y)
parameters <- list(
  x1 = 5,
  x1_2 = 5,
  zets = rnorm(length(simdat$y) - 1, 0, 1),
  zets2 = rnorm(length(simdat$y) - 1, 0, 1),
  log_total_resid_sd = log(2),
  logit_p_proc_var = 0
)

# Set up the model with RTMB and compile the function
obj <- RTMB::MakeADFun(f, parameters, random = c("zets", "zets2"))
# Optimize the parameters to minimize the negative log-likelihood
opt <- nlminb(obj$par, obj$fn, obj$gr)
# Extract the reported values from the model
rep <- obj$report()

# Print the optimized parameters and reported values
print(opt$par)
print(rep)

# Load the MARSS package for state-space modeling
library(MARSS)

# Define state variables and observations for MARSS
X1 <- x
X2 <- x2
X3 <- x3
Y1 <- y
Y3 <- y3
dat <- rbind(Y1, Y3)  # Combine the observations into a data matrix

# Define the MARSS model matrices
Z <- matrix(c(1, 0, 0,  # Observation matrix
              0, 0, 1), nrow = 2, byrow = TRUE)
B <- matrix(c(1, 0, 0,  # State transition matrix
              0, 1, 0,
              1, 1, 0), nrow = 3, byrow = TRUE)
U <- matrix(0, nrow = 3, ncol = 1)  # Control matrix
Q <- "diagonal and equal"  # State covariance matrix
R <- "diagonal and equal"  # Observation covariance matrix
x0 <- matrix(c(X1 = 5, X2 = 5, X3 = 10), nrow = 3)  # Initial states
V0 <- diag(3)  # Initial state covariance

# Create the MARSS model list
mod_list <- list(Z = Z, B = B, U = U, Q = Q, R = R, x0 = x0, V0 = V0, tinitx = 1)

# Fit the MARSS model to the data
fit <- MARSS(dat, model = mod_list)

# Plot the observed data and fitted states from both RTMB and MARSS models
plot(y, type = "b")
lines(x, col = "blue")
lines(rep$x, col = "red")
lines(t(fit$states)[, 1], col = "purple")

plot(y3, type = "b")
lines(x3, col = "blue")
lines(rep$x3, col = "red")
lines(t(fit$states)[, 3], col = "purple")

plot(x2, type = "l", col = "blue")
lines(rep$x2, col = "red")
lines(t(fit$states)[, 2], col = "purple")

# Plot scatter plots with fitted vs. true state values to check model fit
plot(rep$x3 ~ x3)
abline(a = 0, b = 1)
plot(t(fit$states)[, 3] ~ x3)
abline(a = 0, b = 1)
plot(t(fit$states)[, 3] ~ rep$x3)
abline(a = 0, b = 1)

# Compare standard deviations estimated by RTMB and MARSS
sigma_obs
sigma_proc
sqrt(fit$par$R)
sqrt(fit$par$Q)
rep$sigma_obs
rep$sigma_proc

# Calculate and compare root mean square error (RMSE) between true and fitted state variables
sqrt(mean((x - rep$x)^2))
sqrt(mean((x - fit$states[1,])^2))

sqrt(mean((x3 - rep$x3)^2))
sqrt(mean((x3 - fit$states[3,])^2))
