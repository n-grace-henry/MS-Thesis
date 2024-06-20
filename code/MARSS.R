# Load packages 
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(ggplot2)
library(dplyr)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/mass_correct.csv")

# Clean 
data <- data[,-c(1:4)]

#### MARSS model ####

## fully defined observation model

## (1) y_t = Z %*% x_t + a + D %*% d_{t-k} + v_t with v_t ~ MVN(0, R)

## fully defined state (process) model

## (2) x_t = B %*% x_{t-1} + u + C %*% c_{t-h} + w_t with w_t ~ MVN(0, Q)


#### Grace's obs model ####

## y_t = Z %*% x_t + a + v_t with v_t ~ MVN(0, R)

## Notes
## n is the total number of observations (= 3 districts * 3 samples = 9)
## y_t is an [n x 1] vector of obs
## Z is an [n x p] matrix of 1's and 0's that maps the obs onto the states
## a is an [n x 1] col vector of offsets (y-intercepts)
## R is an [n x n] covariance matrix (see below)

## in MARSS() form

## set n & p
nn <- 9
pp <- 3

ZZ <- matrix(0, nn, pp)
ZZ[1:3, 1] <- 1
ZZ[4:6, 2] <- 1
ZZ[7:9, 3] <- 1

AA <- matrix(letters[1:nn], nn, 1)

## errors are independent and identically distributed (IID)
RR <- matrix(list(0), nn, nn)
diag(RR) <- rep("r", nn)


#### Grace's possible state models ####

## we discussed 3 possible versions of (2) above:

## i) random walk

##  x_t = x_{t-1} + w_t with w_t ~ MVN(0, Q)

## ii) biased random walk (where u is the bias or drift)

##  x_t = x_{t-1} + u + w_t with w_t ~ MVN(0, Q)

## iii) stationary or "mean-reverting" (where |diag(B)| < 1)

##  x_t = B %*% x_{t-1} + w_t with w_t ~ MVN(0, Q)

## Notes
## p is the number of fishing districts (= 3)
## in all cases: x_t is a [p x 1] col vector of states;
##               w_t is a [p x 1] col vector of process errors
##               Q is a [p x p] covariance matrix (see below)
## in (ii): u is a [p x 1] col vector of bias terms; they can be the same for
##           all 3 districts or unique by district
## in (iii): B is a [p x p ] matrix with (possibly) unique values down the
##           diagonal and 0's elsewhere

## models in MARSS() form

## 2 options for Q for all 3 state models
## a) independent and identically distributed (IID)
QQ <- matrix(list(0), pp, pp)
diag(QQ) <- rep("q", pp)
## b) identically distributed but not independent (districts marginally synchronous)
QQ <- matrix(list("g"), pp, pp)
diag(QQ) <- rep("q", pp)


## i) random walk

BB <- diag(pp)

UU <- matrix(0, pp, 1)

## define model list for MARSS(); C,c & D,d are all 0 by default
mod_list_1 <- list(
  ## state eqn
  B = BB,
  U = UU,
  Q = QQ,
  ## obs eqn
  Z = ZZ,
  A = AA,
  R = RR
)


## ii) biased random walk (where u is the bias or drift)

## 2 options for u
## a) same bias (trend) everywhere
UU <- matrix("u", pp, 1)
## b) bias (trend) varies by district
UU <- matrix(paste0("u_", seq(pp)), pp, 1)

## define model list for MARSS(); C,c & D,d are all 0 by default
mod_list_2 <- list(
  ## state eqn
  B = BB,
  U = UU,
  Q = QQ,
  ## obs eqn
  Z = ZZ,
  A = AA,
  R = RR
)


## iii) stationary or "mean-reverting" (where |diag(B)| < 1)

BB <- matrix(list(0), pp, pp)
## 2 options for B
## a) all districts have the same degree of mean-reversion
diag(BB) <- "b"
## b) each district has its own degree of mean-reversion
diag(BB) <- paste0("b_", seq(pp))

## define model list for MARSS(); C,c & D,d are all 0 by default
mod_list_3 <- list(
  ## state eqn
  B = BB,
  U = UU,
  Q = QQ,
  ## obs eqn
  Z = ZZ,
  A = AA,
  R = RR
)


#### pseudo-code ####

## 1) pick option for Q (lines 81-87)
## 2) pick option for state model (lines 90+, 109+ or 130+)
## 3) fit model for baseline isotopes
## 4) fit model for TP isotopes
## 5) repeat 1-4 as necessary
## 6) choose "best" model based on AICc
## 7) calculate TP based on estimated states from best models
## 8) fit new model for (7) with possible covariates

## Note that for (8) with covariates, you will need to add `C` (effect(s) of
## covariates on states) and `c` (covariates themselves) to the model lists
## defined above. For example, if we had one covariate (eg, PDO) and we wanted
## it to affect each of 3 districts in the same way, then

CC <- matrix("PDO", pp, 1)

## if we wanted each district to have its own unique effect of PDO, then

CC <- matrix(paste0("PDO_", seq(pp)), pp, 1)

## assume pdo_data is a vector and TT is length of the time series
cc <- matrix(pdo_data, 1, TT)

#### fitting MARSS models ####

## yy is an [n x T] matrix
## mod_list_i is a list from above where i in {1,2,3}

mod_fit <- MARSS(y = yy, model = model_list_i)


## extract estimated states; [p x T] matrix
xx <- mod_fit$states

## extract standard errors of states; [p x T] matrix
xx_SE <- mod_fit$states.se

