# Load libraries 
library(ggplot2)
library(dplyr)

# Load data
size <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA_covariates_2024.csv")
str(size)
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/Ohlberger.csv")

# Delete last row
size <- size[!size$Year %in% c(2023, 2024),]

# Delete last two columns
size <- size[, -c(7, 8)]
