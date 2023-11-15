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

Wood <- 

?subset()

