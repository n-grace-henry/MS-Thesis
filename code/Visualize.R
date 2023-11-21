#this is an r script to do some basic data visualization of the cleaned data
#at this point the data should be drift corrected, compiled, and had all
#duplicates and replicates removed and averaged

setwd("~/Documents/Grad School /CSIA/code")

library(dplyr)
library(readr)
library(ggplot2)

#read in the main data file
data <- read.csv(file="main.data")

#scatter plot of year vs isotope signature
ggplot(data = data, aes(Year, PHE.mean, color = System)) +
       geom_point(size = 3, alpha = 0.7) 

#+geom_smooth(method = "lm")

ggplot(data = data, aes(Year, PHE.mean, color = System)) +
  geom_line(size = 1, alpha = 0.7) 

Wood <- data[data$System =="Wood",]
Kvichak <- data[data$System =="Kvichak",]
Egegik <- data[data$System =="Egegik",]

#plots of age vs isotope sig in the different river systems 
ggplot(data = Wood, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Kvichak, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Egegik, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 
