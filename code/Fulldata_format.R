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

# Write excel files and format by hand 
write_xlsx(data.GLU, path = "~/Documents/GitHub/CSIA_lab_work/data/final/data.GLU.xlsx")
write_xlsx(data.PHE, path = "~/Documents/GitHub/CSIA_lab_work/data/final/data.PHE.xlsx")

# Create column for sample number to distinguish reps and dupes 
data.GLU$Sample <- 1:nrow(data.GLU)

# Get rid of samples that have replicates (keeping replicates)
data.GLU <- data.GLU[!data.GLU$ID1 == "13_W_2" & 
                       !data.GLU$ID1 == "13_W_3",]
data.PHE <- data.PHE[!data.PHE$ID1 == "13_W_2" & 
                       !data.PHE$ID1 == "13_W_3",]
data.GLU <- data.GLU[!data.GLU$ID1 == "89_K_2" &
                       !data.GLU$ID1 == "89_W_2",]
data.PHE <- data.PHE[!data.PHE$ID1 == "89_K_2" &
                       !data.PHE$ID1 == "89_W_2",]

# Delete replicates and triplicates??


# Format: three points per year, every year represented
