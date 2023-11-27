#calculating trophic position 
setwd("~/Documents/Grad School /CSIA/code")

library(dplyr)
library(readr)

#read in the main data file
data <- read.csv(file="main.data")

#TP = 1 + abs((d15NGlu - d15NPhe -beta)/TDFglu-phe)