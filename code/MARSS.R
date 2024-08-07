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
#data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")

# Add column that takes last two digits of ID1
data.full$Rep <- substr(data.full$ID1, 8, 8)

# Change ID1 column to not show reps
data.full$ID1 <- substr(data.full$ID1, 1, 6)

#### Non-averaged triplicates per system (max of 6 injections per sample ####
# Subset data for age 2 and only PHE
PHE <- data.full[data.full$Age == "2" &
                   data.full$AAID == "PHE", c("Year", "adj", "System", "Age", "ID1", "Rep")]

# Wood
PHE.W <- PHE[PHE$System == "Wood", c("Year", "adj", "ID1", "Rep")]

# Egegik 
PHE.E <- PHE[PHE$System == "Egegik", c("Year", "adj", "System", "ID1", "Rep")]

# Kvichak
PHE.K <- PHE[PHE$System == "Kvichak", c("Year", "adj", "System", "ID1", "Rep")]

# Subset data for age 2 and only GLU 
GLU <- data.full[data.full$Age == "2" &
                   data.full$AAID == "GLU", c("Year", "adj", "System", "Age", "ID1", "Rep")]

# Wood
GLU.W <- GLU[GLU$System == "Wood", c("Year", "adj", "System", "ID1", "Rep")]

# Egegik
GLU.E <- GLU[GLU$System == "Egegik", c("Year", "adj", "System", "ID1", "Rep")]

# Kvichak
GLU.K <- GLU[GLU$System == "Kvichak", c("Year", "adj", "System", "ID1", "Rep")]

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

# Convert wide back to long 
PHE.long <- pivot_longer(PHE.wide, cols = c(2:4), names_to = "sample_num", values_to = "adj")

#### Convert to time series data ####
# Get only value column 
PHE.W.data <- PHE.long$adj
PHE.long.ts <- ts(PHE.W.data, start = 1965, end = 2022, frequency = 3)

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

fit.W <- MARSS(PHE.long.ts, model = mod.list)
years <- rep(1965:2022, each = 3)
years <- years[1:172]

plot(PHE.long.ts, type = "p", col = "blue", xlab = "Year", ylab = "PHE.mean", main = "Time Series Plot")
lines(years, fit.W$states[1,], col = "red")




