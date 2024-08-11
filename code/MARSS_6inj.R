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
# Create a new data frame with the desired format for Wood PHE
PHE.W <- PHE[PHE$System == "Wood", c("Year", "adj", "ID1", "Rep")]
samples <- 6
PHE.W.NA <- PHE.W %>%
  group_by(Year) %>%
  mutate(Sample_Number = row_number()) %>%
  complete(Sample_Number = 1:samples) %>%
  arrange(Year, Sample_Number, ID1, Rep) %>%
  select(Year, adj, ID1, Rep)


