setwd("~/Documents/Grad School /CSIA/code")

library(dplyr)
library(readr)
library(ggplot2)

#read in the main data file
data <- read.csv(file="main.data")

#average points that have replicates
a <- subset(data, Sample.ID == "22_K_3")
b <- a[,6:15]
mean(b[,1])

K322 <- vector(mode="numeric", length=10)
for(i in 1:10){
  K322[i] <- mean(b[,i])
}
c <- append(c(1, "22_K_3", "2022", "Kvichak", "3"), K322)

norep <- data[!data$Sample.ID=="22_K_3",]
data <- rbind(norep, c)

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
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm")

ggplot(data = Kvichak, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Egegik, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 
