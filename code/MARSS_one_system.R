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
# Subset Kvichak PHE 
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
# Subset Egegik PHE
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]

# Subset data for age 2 and only PHE
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "System", "Age", "adj", "ID1", "Rep")]

# Subset Wood GLU
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
# Subset Kvichak GLU
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
# Subset Egegik GLU
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]

# Function to get all data to wide format 
wide <- function(data){
  # Full years to represent in data
  full_years <- seq(1965, 2022, by = 1)
  
  # Format data: six injections per year, every year represented
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
 
# Adjust the Z matrix to assume one state for all systems
ZZ <- matrix(1, 18, 1)

# Modify the model specifications
mod.list <- list(
  B = "identity",  
  U = "zero",          
  Q = "diagonal and equal",         
  Z = ZZ,
  A = "scaling",    
  R = "diagonal and unequal",
  x0 = matrix("mu", nrow = 1, ncol = 1),  
  tinitx = 0             
)

# Fit PHE model 
PHE.fit.one <- MARSS(all.PHE, model = mod.list)
autoplot(PHE.fit.one)

# Fit GLU model 
GLU.fit.one <- MARSS(all.GLU, model = mod.list)
autoplot(GLU.fit.one)

# Subset states
PHE.state <- PHE.fit.one$states
GLU.state <- GLU.fit.one$states

# Plot overall PHE state 
years <- seq(1965, 2022, by = 1)
plot(x = PHE$Year, y = PHE$adj, type = "p", pch = 16, col = "black", xlab = "Year", ylab = expression(paste("Phenylalanine ", delta^{15}, "N ")), main = "All Phenylalanine")
lines(x = years, y = PHE.state, type = "l", lwd = 2, col = "blue")

# Plot overall GLU state
plot(x = GLU$Year, y = GLU$adj, type = "p", pch = 16, col = "black", xlab = "Year", ylab = expression(paste("Glutamic Acid ", delta^{15}, "N")), main = "All Glutamic Acid")
lines(x = years, y = GLU.state, type = "l", lwd = 2, col = "red")

# Trophic position calculations
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

tp <- (((GLU.state - PHE.state)-beta)/TDF) + 1

# Plot TP 
plot(x = years, y = tp, type = "l", lwd =2, col = "black", xlab = "Year", ylab = "Trophic Position", main = "Overall Trophic Position")

# Anomaly plot for PHE 
PHE.t <- as.data.frame(t(PHE.state))
PHE.t$Year <- years
PHE.t$Mean <- vector(length = length(PHE.t$Year))
PHE.t$Anomaly <- vector(length = length(PHE.t$Year))

for(i in 1:length(PHE.t$Year)){
  PHE.t[i, "Mean"] <- mean(PHE.t[,1])
  PHE.t[i, "Anomaly"] <- PHE.t$X1[i] - PHE.t$Mean[i]
}

ggplot(PHE.t, aes(x = Year, y = Anomaly)) +
  geom_ribbon(aes(ymin = pmin(Anomaly, 0), ymax = 0), fill = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(Anomaly, 0)), fill = "grey", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(min(PHE.t$Year), max(PHE.t$Year), by = 3)) +
  labs(title = "Phenylalanine Anomaly Plot",
       x = "Year",
       y = "Anomaly (Value - Long Term Mean") +
  theme_minimal()

# Anomaly plot for GLU
GLU.t <- as.data.frame(t(GLU.state))
GLU.t$Year <- years
GLU.t$Mean <- vector(length = length(GLU.t$Year))
GLU.t$Anomaly <- vector(length = length(GLU.t$Year))

for(i in 1:length(GLU.t$Year)){
  GLU.t[i, "Mean"] <- mean(GLU.t[,1])
  GLU.t[i, "Anomaly"] <- GLU.t$X1[i] - GLU.t$Mean[i]
}

ggplot(GLU.t, aes(x = Year, y = Anomaly)) +
  geom_ribbon(aes(ymin = pmin(Anomaly, 0), ymax = 0), fill = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(Anomaly, 0)), fill = "grey", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(min(tp.t$Year), max(tp.t$Year), by = 3)) +
  labs(title = "Glutamic Acid Anomaly Plot",
       x = "Year",
       y = "Anomaly (Value - Long Term Mean") +
  theme_minimal()

# Anomaly plot for TP 
tp.t <- as.data.frame(t(tp))
tp.t$Year <- years
tp.t$Mean <- vector(length = length(tp.t$Year))
tp.t$Anomaly <- vector(length = length(tp.t$Year))

for(i in 1:length(tp.t$Year)){
  tp.t[i, "Mean"] <- mean(tp.t[,1])
  tp.t[i, "Anomaly"] <- tp.t$X1[i] - tp.t$Mean[i]
}

ggplot(tp.t, aes(x = Year, y = Anomaly)) +
  geom_ribbon(aes(ymin = pmin(Anomaly, 0), ymax = 0), fill = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(Anomaly, 0)), fill = "grey", alpha = 0.5) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(min(tp.t$Year), max(tp.t$Year), by = 3)) +
  labs(title = "Trophic Position Anomaly Plot",
       x = "Year",
       y = "Anomaly (Value - Long Term Mean)") +
  theme_minimal()





