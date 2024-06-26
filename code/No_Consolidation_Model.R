# This is a script to generate a csv file that has data corrected but never consolidated

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)

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

# Plot data - PHE
plot(x = data[data$AAID == "PHE", "Year"],
     y = data[data$AAID == "PHE", "d15N.correct"])

# Plot data = GLU
plot(x = data[data$AAID == "GLU", "Year"],
     y = data[data$AAID == "GLU", "d15N.correct"])

# PHE data frame 
PHE <- data[data$AAID == "PHE", "d15N.correct"]
phe.year <- as.integer(data[data$AAID == "PHE", "Year"]) #not sure this will work
phe.sys <- data[data$AAID == "PHE", "System"]
phe.age <- data[data$AAID == "PHE", "Age"]
PHE.df <- data.frame(PHE, phe.year, phe.sys, phe.age)
str(PHE.df)

# GLU data frame
GLU <- data[data$AAID == "GLU", "d15N.correct"]
glu.year <- data[data$AAID == "GLU", "Year"]
glu.sys <- data[data$AAID == "GLU", "System"]
glu.age <- data[data$AAID == "GLU", "Age"]
GLU.df <- data.frame(GLU, glu.year, glu.sys, glu.age)

# PHE GAM
gam_phe <- gam(PHE ~ s(phe.year), data = PHE.df, method = "REML")
summary(gam_phe)


