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

# Create full data set 



#copied from other script 
# Create a complete sequence of years
years <- seq(1965, 2022, by = 1)

# Create a complete data frame for each river system
complete_df <- expand.grid(Year = years, System = unique(data$System), Age = unique(data$Age))

# Merge with the original data to insert NA values for missing data
merged_df <- left_join(complete_df, data, by = c("Year", "System", "Age"))
merged_df <- merged_df[order(merged_df$Year, merged_df$Age, merged_df$System), ]



