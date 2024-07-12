# Load packages
library(dplyr)
library(writexl)

# Read in data
data.full <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/full.csv")

# Order data by year
data.full <- data.full %>% arrange(Year)

# Separate into GLU and PHE data, will combine again later 
data.GLU <- data.full[data.full$AAID == "GLU",]
data.PHE <- data.full[data.full$AAID == "PHE",]

# Delete all replicates and duplicates
index.GLU <- which(substr(data.GLU$ID1, 8, 8) != "")
index.PHE <- which(substr(data.PHE$ID1, 8, 8) != "")

data.GLU <- data.GLU[-index.GLU,]
data.PHE <- data.PHE[-index.PHE,]

#

