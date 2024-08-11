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

# Full years to represent in data
full_years <- seq(1965, 2022, by = 1)

# Format data: three injections per year, every year represented
long.all.yr <- PHE.W %>%
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
wide <- long.all.yr %>%
  pivot_wider(names_from = SampleNumber, values_from = adj, names_prefix = "Inj")

# Transpose wide format
wide.t <- t(wide)
wide.t <- wide.t[-1,]



