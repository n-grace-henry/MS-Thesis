# Load packages 
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(ggplot2)
library(dplyr)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/mass_correct.csv")

# Clean 
data <- data[,-c(1:4)]

# Add year column from ID1
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

# Add age column
data$Age <- substr(data$ID1, 6, 6)

# Put into 9 by t matrix for MARSS
data[data$adj == ]
