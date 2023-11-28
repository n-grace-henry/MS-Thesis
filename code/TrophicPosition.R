#calculating trophic position 
setwd("~/Documents/Grad School /CSIA/code")

library(dplyr)
library(readr)

#read in the main data file
data <- read.csv(file="main.data")

#define beta and TDF values, this can be changed later if necessary 
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

#make an empty data frame to fill with Sample.ID and trophic position 
tp <- data.frame(matrix(nrow = length(data$Sample.ID), ncol = 2))
tp <-setNames(tp, c("Sample.ID","Trophic.Position"))

#for loop to calculate trophic position and fill data frame
for(i in length(data$Sample.ID)){
  (data$GLU.mean[i]-data$PHE.mean[i]-beta)/TDF)
}

data$PHE.mean[1]
