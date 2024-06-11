# This is a script to generate a csv file that has data corrected but never consolidated

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv("mass_correct.csv")

# Clean data for easy visualization 
data <- data[,-c(1:4)]

# Add year column 
year.2digit <- substr(data$ID1, 1, 2)
year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}
data$Year <- year

# Add system column 
sys <- substr(data$ID1, 4, 4)
system <- vector(mode="character")
for(i in 1:length(sys)){
  if(sys[i] == "W"){
    system[i] <- "Wood"
  } else if(sys[i] == "K"){
    system[i] <- "Kvichak"
  }  else{
    system[i] <- "Egegik"
  }
}
data$System <- system

# Add age class column 
data$Age <- substr(data$ID1, 6, 6)

# Plot data
ggplot(data, aes(x = ))