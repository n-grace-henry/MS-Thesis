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

# Wood
PHE.W <- PHE[PHE$System == "Wood", c("Year", "adj", "ID1", "Rep")]
# Kvichak
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
# Egegik
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]

# Subset data for age 2 and only GLU
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "System", "Age", "adj", "ID1", "Rep")]

# Wood
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
# Kvichak
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
# Egegik
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]

# Create function to format data correctly 
format <- function(data) {
  # Full years to represent in data
  full_years <- seq(1965, 2022, by = 1)
  
  # Format data: three six per year, every year represented
  long <- data %>%
    arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID, and Rep
    group_by(Year) %>%  # Group by Year
    mutate(SampleNumber = as.character(row_number())) %>%  # Assign and convert unique sample numbers to characters
    select(-ID1, -Rep) %>%  # Remove columns ID and Rep
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
  
  # Transpose wide format
  wide.t <- t(wide)
  wide.t <- wide.t[-1,]
  
  return(wide.t)
}

# Get all systems in correct format for MARSS
PHE.wide.W <- format(PHE.W)
PHE.wide.K <- format(PHE.K)
PHE.wide.E <- format(PHE.E)

GLU.wide.W <- format(GLU.W)
GLU.wide.K <- format(GLU.K)
GLU.wide.E <- format(GLU.E)

# One data frame for PHE 
all.PHE <- rbind(PHE.wide.W, PHE.wide.K, PHE.wide.E)

# One data frame for GLU 
all.GLU <- rbind(GLU.wide.W, GLU.wide.K, GLU.wide.E)

# Define Z matrix
ZZ <- matrix(0, 18, 3) 
ZZ[1:6, 1] <- 1
ZZ[7:12, 2] <- 1
ZZ[13:18, 3] <- 1

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

# Fitting the model for PHE
fit.phe <- MARSS(all.PHE, model = mod.list)
autoplot(fit.phe)

# Fitting the model for GLU
fit.glu <- MARSS(all.GLU, model = mod.list)
autoplot(fit.glu)

# Subset states
PHE.state.W <- fit.phe$states[1,]
PHE.state.K <- fit.phe$states[2,]
PHE.state.E <- fit.phe$states[3,]

GLU.states.W <- fit.glu$states[1,]
GLU.states.K <- fit.glu$states[2,]
GLU.states.E <- fit.glu$states[3,]

# Plot PHE states 
years <- seq(1965, 2022, by = 1)
# Wood
plot(x = PHE.W$Year, y = PHE.W$adj, type = "p", pch = 16, col = "black", cex = 1.2, xlab = "Year", ylab = expression(paste("Phenylalanine ", delta^{15}, "N/ ", delta^{14}, "N")),main = "Wood Phenylalanine")
lines(x = years, y = PHE.state.W, type = "l", col = "red", lwd = 2)
legend("topright", legend = c("Raw Data", "PHE State"), col = c("black", "red"), pch = c(16, NA),lty = c(NA, 1), lwd = c(NA, 2))
# Kvichak
plot(x = PHE.K$Year, y = PHE.K$adj, type = "p", pch = 16, col = "black", cex = 1.2, xlab = "Year", ylab = expression(paste("Phenylalanine ", delta^{15}, "N/ ", delta^{14}, "N")), main = "Kvichak Phenylalanine")
lines(x = years, y = PHE.state.K, type = "l", col = "red", lwd = 2)
# Egegik 
plot(x = PHE.E$Year, y = PHE.E$adj, type = "p", pch = 16, col = "black", cex = 1.2, xlab = "Year", ylab = expression(paste("Phenylalanine ", delta^{15}, "N/ ", delta^{14}, "N")), main = "Egegik Phenylalanine")
lines(x = years, y = PHE.state.E, col = "red", lwd = 2)

# Plot GLU states
# Wood
plot(GLU.W$Year, GLU.W$adj, type = "p", pch = 16, col = "black", cex = 1.2, xlab = "Year", ylab = expression(paste("Glutamic Acid ", delta^{15}, "N/ ", delta^{14}, "N")), main = "Wood Glutamic Acid")
lines(x = years, y = GLU.states.W, type = "l", col = "red", lwd = 2)
legend("bottomright", legend = c("Raw Data", "GLU State"), col = c("black", "red"), pch = c(16, NA),lty = c(NA, 1), lwd = c(NA, 2))
# Kvichak 
plot(GLU.K$Year, GLU.K$adj, type = "p", pch = 16, col = "black", cex = 1.2, xlab = "Year", ylab = expression(paste("Glutamic Acid ", delta^{15}, "N/ ", delta^{14}, "N")), main = "Kvichak Glutamic Acid")
lines(x = years, y = GLU.states.K, type = "l", col = "red", lwd = 2)
# Egegik 
plot(GLU.E$Year, GLU.E$adj, type = "p", pch = 16, col = "black", cex = 1.2, xlab = "Year", ylab = expression(paste("Glutamic Acid ", delta^{15}, "N/ ", delta^{14}, "N")), main = "Egegik Glutamic Acid")
lines(x = years, y = GLU.states.E, type = "l", col = "red", lwd = 2)

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





