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
plot(x = PHE.W$Year, y = PHE.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Subset Kvichak PHE 
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.K$Year, y = PHE.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Subset Egegik PHE
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.E$Year, y = PHE.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")

# Subset data for age 2 and only PHE
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "System", "Age", "adj", "ID1", "Rep")]

# Subset Wood GLU
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.W$Year, y = GLU.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU", main = "GLU Wood")

# Subset Kvichak GLU
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.K$Year, y = GLU.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU", main = "GLU Kvichak")

# Subset Egegik GLU
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.E$Year, y = GLU.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU", main = "GLU Egegik")

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

# Scale PHE and GLU data by subtracting the mean
all.PHE.scaled <- apply(all.PHE, MARGIN = 1, FUN = mean, na.rm = TRUE)
all.GLU.scaled <- apply(all.GLU, MARGIN = 1, FUN = mean, na.rm = TRUE)

# Substract mean from data
all.PHE.centered <- sweep(all.PHE, MARGIN = 1, STATS = all.PHE.scaled, FUN = "-")
all.GLU.centered <- sweep(all.GLU, MARGIN = 1, STATS = all.GLU.scaled, FUN = "-")

# Define Z matrix
ZZ <- matrix(0, 9, 3) 
ZZ[1:3, 1] <- 1
ZZ[4:6, 2] <- 1
ZZ[7:9, 3] <- 1

# PHE model specifications
mod.list <- list(
  B = "identity",  
  U = "zero",          
  Q = "diagonal and equal",         
  Z = ZZ,
  A = "scaling",    
  R = "diagonal and unequal",
  x0 = matrix(c("mu1", "mu2", "mu3"), nrow = 3, ncol = 1),      
  tinitx = 0             
)

# Fit PHE model 
PHE.fit <- MARSS(all.PHE, model = mod.list)
autoplot(PHE.fit)

# Calculate AICC value of PHE.fit model 
AIC <- PHE.fit$AIC
k <- PHE.fit$num.params
n <- nrow(all.PHE) * ncol(all.PHE)
AICC <- AIC + (2 * k * (k + 1)) / (n - k - 1)

# Fit GLU model 
GLU.fit <- MARSS(all.GLU, model = mod.list)
autoplot(GLU.fit)

# Calculate AICC value of GLU.fit model
AIC.GLU <- GLU.fit$AIC
k.GLU <- GLU.fit$num.params
n.GLU <- nrow(all.GLU) * ncol(all.GLU)
AICC.GLU <- AIC.GLU + (2 * k.GLU * (k.GLU + 1)) / (n.GLU - k.GLU - 1)

# Subset states
PHE.state.W <- PHE.fit$states[1,]
PHE.state.K <- PHE.fit$states[2,]
PHE.state.E <- PHE.fit$states[3,]

GLU.states.W <- GLU.fit$states[1,]
GLU.states.K <- GLU.fit$states[2,]
GLU.states.E <- GLU.fit$states[3,]

# Plot PHE states 
years <- seq(1965, 2022, by = 1)
# Wood
plot(x = PHE.W$Year, y = PHE.W$adj, type = "p", col = "black", xlab = "Year", ylab = "PHE.mean", main = "PHE Wood")
lines(x = years, y = PHE.state.W, type = "l", col = "blue", xlab = "Year", ylab = "PHE State", main = "PHE Wood")
# Kvichak
plot(x = PHE.K$Year, y = PHE.K$adj, type = "p", col = "black", xlab = "Year", ylab = "PHE.mean", main = "PHE Kvichak")
lines(x = years, y = PHE.state.K, type = "l", col = "blue", xlab = "Year", ylab = "PHE State", main = "PHE Kvichak")
# Egegik
plot(x = PHE.E$Year, y = PHE.E$adj, type = "p", col = "black", xlab = "Year", ylab = "PHE.mean", main = "PHE Egegik")
lines(x = years, y = PHE.state.E, type = "l", col = "blue", xlab = "Year", ylab = "PHE State", main = "PHE Egegik")

# Plot GLU states
# Wood
plot(x = GLU.W$Year, y = GLU.W$adj, type = "p", col = "black", xlab = "Year", ylab = "GLU.mean", main = "GLU Wood")
lines(x = years, y = GLU.states.W, type = "l", col = "blue", xlab = "Year", ylab = "GLU State", main = "GLU Wood")
# Kvichak
plot(x = GLU.K$Year, y = GLU.K$adj, type = "p", col = "black", xlab = "Year", ylab = "GLU.mean", main = "GLU Kvichak")
lines(x = years, y = GLU.states.K, type = "l", col = "blue", xlab = "Year", ylab = "GLU State", main = "GLU Kvichak")
# Egegik
plot(x = GLU.E$Year, y = GLU.E$adj, type = "p", col = "black", xlab = "Year", ylab = "GLU.mean", main = "GLU Egegik")
lines(x = years, y = GLU.states.E, type = "l", col = "blue", xlab = "Year", ylab = "GLU State", main = "GLU Egegik")

# Trophic position calculations
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

tp.W <- (((GLU.states.W - PHE.state.W)-beta)/TDF) + 1
tp.K <- (((GLU.states.K - PHE.state.K)-beta)/TDF) + 1
tp.E <- (((GLU.states.E - PHE.state.E)-beta)/TDF) + 1

# Put into one data frame for plotting 
tp <- (cbind(years, tp.W, tp.K, tp.E))
colnames(tp) <- c("Year", "Wood", "Egegik", "Kvichak")
tp <- as.data.frame(tp)

# Convert to long for plotting 
tp_long <- pivot_longer(tp, cols = -Year, names_to = "System", values_to = "TP")

# Plot all
ggplot(tp_long, aes(x = Year, y = TP, color = System)) +
  geom_line(size = 1.5) +
  labs(title = "Trophic Position Over Time",
       x = "Year",
       y = "Trophic Position") +
  scale_x_continuous(breaks = seq(min(tp_long$Year), max(tp_long$Year), by = 3), 
                     labels = function(x) as.character(x)) +  # Custom labeling function
  theme_minimal()

# Plot Wood 
ggplot(tp, aes(x = Year, y = Wood)) +
  geom_line() +
  labs(title = "Wood Trophic Position Over Time",
       x = "Year",
       y = "Trophic Position") +
  theme_minimal()

# Plot Egegik
ggplot(tp, aes(x = Year, y = Egegik)) +
  geom_line() +
  labs(title = "Egegik Trophic Position Over Time",
       x = "Year",
       y = "Trophic Position") +
  theme_minimal()

# Plot Kvichak
ggplot(tp, aes(x = Year, y = Kvichak)) +
  geom_line() +
  labs(title = "Kvichak Trophic Position Over Time",
       x = "Year",
       y = "Trophic Position") +
  theme_minimal()





