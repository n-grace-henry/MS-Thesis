#this is an r script to do some basic data visualization of the cleaned data
#at this point the data should be drift corrected, compiled, and had all
#duplicates and replicates removed and averaged

setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

library(dplyr)
library(readr)
library(ggplot2)

#read in the main data file
data <- read.csv(file="main.clean.csv")

#scatter plot of year vs isotope signature
ggplot(data = data, aes(Year, PHE.mean, color = System)) +
       geom_point(size = 3, alpha = 0.7) 

#+geom_smooth(method = "lm")

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

#make a graph showing total number of samples/system/year 

#trophic position graphs

trophic.data <- read.csv(file = "trophic.position")

ggplot(data = trophic.data, aes(Year, Trophic.Position, color = System)) +
  geom_point(size = 3, alpha = 0.7) 


#difference between PHE and GLU
diff <- data$GLU.mean-data$PHE.mean
data$difference <- diff

ggplot(data = data, aes(Year, difference, color = System)) +
  geom_point(size = 3, alpha = 0.7) 


#separating by age class
data.age2 <- data[data$Age == "2",]
data.age3 <- data[data$Age == "3",]

ggplot(data = data.age2, aes(Year, difference, color = System)) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = data.age3, aes(Year, difference, color = System)) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = data, aes(Year, GLU.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7) 


