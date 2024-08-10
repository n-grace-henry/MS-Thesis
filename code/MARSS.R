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

# Wood river data
# Format long and wide data frames for Wood system
PHE.W <- PHE[PHE$System == "Wood", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.W$Year, y = PHE.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")

# Format data to transposed wide for 3 injections 
PHE.W.long <- PHE.W %>%
  arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID1, and Rep
  filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
  group_by(Year) %>%  # Group by Year
  mutate(SampleNumber = row_number()) %>%  # Assign unique sample numbers
  select(-ID1, -Rep)  # Remove columns ID1 and Rep

# Convert to wide format
PHE.wide <- PHE.W.long %>%
  pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")

# Transpose
PHE.wide.t <- t(PHE.wide)
PHE.wide.t <- PHE.wide.t[-1,]

# Run Wood PHE model 
mod.list.1 <- list(
  B = matrix(1),           # State transition matrix
  U = matrix(0),           # No deterministic trend
  Q = matrix("q"),         # Process noise covariance
  Z = matrix(1, 3, 1),     # Observation matrix with 3 observations per time point
  A = matrix(0, 3, 1),     # No observation bias, correct dimensions
  R = "diagonal and equal",# Observation noise structure (diagonal and equal)
  x0 = matrix("mu"),       # Initial state estimate
  tinitx = 0               # Initial time point
)

# Fitting the model
fit.1 <- MARSS(PHE.wide.t, model = mod.list.1)
autoplot(fit.1)

#### Write function to do model for each system ####
model <- function(data){
  
  # Format data to transposed wide for 3 injections 
  long <- data %>%
    arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID1, and Rep
    filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
    group_by(Year) %>%  # Group by Year
    mutate(SampleNumber = row_number()) %>%  # Assign unique sample numbers
    select(-ID1, -Rep)  # Remove columns ID1 and Rep
  
  # Convert to wide format
  wide <- long %>%
    pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")
  
  # Transpose
  wide.t <- t(wide)
  wide.t <- wide.t[-1,]
  
  # Run Wood PHE model 
  mod.list.1 <- list(
    B = matrix(1),           # State transition matrix
    U = matrix(0),           # No deterministic trend
    Q = matrix("q"),         # Process noise covariance
    Z = matrix(1, 3, 1),     # Observation matrix with 3 observations per time point
    A = matrix(0, 3, 1),     # No observation bias, correct dimensions
    R = "diagonal and equal",# Observation noise structure (diagonal and equal)
    x0 = matrix("mu"),       # Initial state estimate
    tinitx = 0               # Initial time point
  )
  
  # Fitting the model
  fit <- MARSS(wide.t, model = mod.list.1)
  plots <- autoplot(fit)
  
  # Print plots
  return(plots)
}

#### Egegik PHE ####
# Egegik 
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = PHE.E$Year, y = PHE.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
model(PHE.E)

#### Kvichak PHE ####
# Kvichak
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(PHE.K$Year, PHE.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
model(PHE.K)


#### GLU ####
# Subset data for age 2 and only GLU 
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "adj", "System", "Age", "ID1", "Rep")]

#### Wood GLU ####
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.W$Year, y = GLU.W$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "Time Series Plot")
model(GLU.W)

#### Egegik ####
 # Format data to transposed wide for 3 injections 
  long <- data %>%
    arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID1, and Rep
    filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
    group_by(Year) %>%  # Group by Year
    mutate(SampleNumber = row_number()) %>%  # Assign unique sample numbers
    select(-ID1, -Rep)  # Remove columns ID1 and Rep
  
  # Convert to wide format
  wide <- long %>%
    pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")
  
  # Transpose
  wide.t <- t(wide)
  wide.t <- wide.t[-1,]
   <- GLU[GLU$System == "Egegik", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.E$Year, y = GLU.E$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "Time Series Plot")
model(GLU.E)


# Format data to transposed wide for 3 injections 
long <- data %>%
  arrange(Year, ID1, Rep) %>%  # Arrange data by Year, ID1, and Rep
  filter(!Rep %in% c("R", "a")) %>%  # Filter out unwanted replicates
  group_by(Year) %>%  # Group by Year
  mutate(SampleNumber = row_number()) %>%  # Assign unique sample numbers
  select(-ID1, -Rep)  # Remove columns ID1 and Rep

# Convert to wide format
wide <- long %>%
  pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")

# Transpose
wide.t <- t(wide)
wide.t <- wide.t[-1,]




# Kvichak
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "ID1", "Rep")]
plot(x = GLU.K$Year, y = GLU.K$adj, type = "p", col = "blue", xlab = "Year", ylab = "GLU.mean", main = "Time Series Plot")
model(GLU.K)

# Full df with NAs where there is missing data for each AA
# Determine the maximum number of samples per year
max_samples <- 6

# Create a new data frame with the desired format for Wood PHE
PHE.W.NA <- PHE.W %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:max_samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)

# Create a new data frame with the desired format for Egegik PHE 
PHE.E.NA <- PHE.E %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:max_samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)

# Create a new data frame with the desired format for Kvichak PHE
PHE.K.NA <- PHE.K %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:max_samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)

# Rep for GLU Wood 
GLU.W.NA <- GLU.W %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:max_samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)

# Rep for GLU Egegik 
GLU.E.NA <- GLU.E %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:max_samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)

# Rep for GLU Kvichak
GLU.K.NA <- GLU.K %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:max_samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)

# Pivot wider with missing years PHE.W
#Subset to get rid of samples with R 
PHE.W.test <- PHE.W[!PHE.W$Rep %in% c("R", "a"),]

PHE.long.edit <- PHE.W.test %>%
  group_by(Year) %>%
  mutate(sample_num = row_number()) %>%
  ungroup()

PHE.wide.edit <- PHE.long.edit %>%
  pivot_wider(names_from = sample_num, values_from = adj, names_prefix = "Value_")

# get rid of extra columns 
PHE.wide.edit <- PHE.wide.edit[, -c(2,3)]

# Transpose
wide.t <- t(PHE.wide.edit)
wide.t <- wide.t[-1,]

#### Upload formatted data from excel ####
PHE.long <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/PHE.long/PHE.W.csv")
PHE.long <- PHE.long[, !names(PHE.long) %in% c("ID1", "Rep")]

#### Pivot Wider to get columns as injections ####
# Wood PHE, add identifier
PHE.long <- PHE.long %>%
  group_by(Year) %>%
  mutate(sample_num = row_number()) %>%
  ungroup()

# Reshape the data frame to wide format
PHE.wide <- PHE.long %>%
  pivot_wider(names_from = sample_num, values_from = adj, names_prefix = "Value_")

# Only keep necessary columns
PHE.wide <- PHE.wide[, 1:7]

# Get back to only 3 injections per year
PHE.wide <- PHE.wide[, -c(5:7)]
wide.t <- t(PHE.wide)
wide.t <- wide.t[-1,]

# Change wide data frame so that there are no years with NAs




#### Univariate State-Space Analysis for each system ####
# Wood
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

fit.W <- MARSS(wide.t, model = mod.list)
years <- rep(1965:2022, each = 3)
years <- years[1:172]

plot(x = PHE.long$Year, y= PHE.long$adj, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(years, fit.W$states[1,], col = "red")

# Trying textbook code 
mod.list.0 <- list(B = matrix(1), 
                   U = matrix("u"), 
                   Q = matrix("q"), 
                   Z = matrix(1, 3, 1), 
                   A = "scaling", 
                   R = "diagonal and unequal", 
                   x0 = matrix("mu"), 
                   tinitx = 0)
fit.0 <- MARSS(wide.t, model = mod.list.0)
autoplot(fit.1)

mod.list.1 <- list(B = matrix(1), 
                   U = matrix("u"), 
                   Q = matrix("q"), 
                   Z = matrix(1, 3, 1), 
                   A = "scaling", 
                   R = "diagonal and equal", 
                   x0 = matrix("mu"), 
                   tinitx = 0)
fit.1 <- MARSS(wide.t, model = mod.list.1)

mod.list.1 <- list(
  B = matrix(1),           # State transition matrix
  U = matrix(0),           # No deterministic trend
  Q = matrix("q"),         # Process noise covariance
  Z = matrix(1, 3, 1),     # Observation matrix with 3 observations per time point
  A = matrix(0, 3, 1),     # No observation bias, correct dimensions
  R = "diagonal and equal",# Observation noise structure (diagonal and equal)
  x0 = matrix("mu"),       # Initial state estimate
  tinitx = 0               # Initial time point
)

# Fitting the model
fit.1 <- MARSS(wide.t, model = mod.list.1)
autoplot(fit.1)



