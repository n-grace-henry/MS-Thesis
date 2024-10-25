library(tidyverse)

setwd("~/Documents/GitHub/CSIA_lab_work/data/ASL_ADFG")
data <- read.csv(file = "ASL_formatted_BristolBay.csv")
unique(data$Location)

small <- data %>% 
  filter(ASLProjectType == "escapement" & 
           Location == c("Wood River","Egegik River", "Kvichak River") & 
           Species == "Sockeye")
  
# Make column of year
small$Year <- as.numeric(substr(small$sampleDate, 1, 4))

# Get rid of every row where year is NA
small <- small %>% 
  filter(!is.na(Year))

# Choose only the relevant years
small <- small %>% 
  filter(Year == c(1965, 1967,1968, 1971, 1974, 1977, 1980, 1983, 1984, 1986, 1989, 1992, 1993, 1995, 1998, 2001, 2004))

unique(small$cardNo)




