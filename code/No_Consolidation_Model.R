# This is a script to generate a csv file that has data corrected but never consolidated

# Load packages
library(dplyr)

# Load data
setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv("data_full.csv")

# Mass correct 

a <- read.csv("mass_correct_full.csv")
