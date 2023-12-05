#this is an r script to do some basic data visualization of the cleaned data
#at this point the data should be drift corrected, compiled, and had all
#duplicates and replicates removed and averaged 

setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

library(dplyr)
library(readr)
library(ggplot2)

#read in the main data file
data <- read.csv(file="main.trophic.csv")

#scatter plot of year vs isotope signature
ggplot(data = data, aes(Year, PHE.mean, color = System)) +
       geom_point(size = 3, alpha = 0.7) 


Wood <- data[data$System =="Wood",]
Kvichak <- data[data$System =="Kvichak",]
Egegik <- data[data$System =="Egegik",]

#standard deviations through time 
ggplot(data = data, aes(Year, GLU.sd, color = System)) +
  geom_line(size = 3, alpha = 0.7) 

#plots of age vs isotope sig in the different river systems 
ggplot(data = Wood, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Kvichak, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Egegik, aes(Year, PHE.mean, color = as.character(Age))) +
  geom_point(size = 3, alpha = 0.7) 

#make a graph showing total number of samples/system/year 
test.data <- data[,2:4]

ggplot(test.data, aes(x = Year, fill = System)) +
  geom_histogram() + 
  labs(title = "Sample Distribution",
       x = "Year",
       y = "Number of Samples")

#trophic position graphs
ggplot(data = data, aes(Year, Trophic.Position, color = System)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Trophic Position Through Time", 
       x = "Year",
       y = "Trophic Position")

ggplot(data = Wood, aes(Year, Trophic.Position, color = Age)) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Kvichak, aes(Year, Trophic.Position, color = Age)) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = Egegik, aes(Year, Trophic.Position, color = Age)) +
  geom_point(size = 3, alpha = 0.7) 


#difference between PHE and GLU
diff <- data$GLU.mean-data$PHE.mean
data$difference <- diff

ggplot(data = data, aes(Year, difference, color = System)) +
  geom_point(size = 3, alpha = 0.7) 


#separating by age class
data.age2 <- data[data$Age == "2",]
data.age3 <- data[data$Age == "3",]

ggplot(data = data, aes(Year, PHE.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Phenylalanine Shifts Through Time",
       x = "Year",
       y = "PHE d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) 

ggplot(data = data, aes(Year, PHE.mean)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Phenylalanine Shifts Through Time",
       x = "Year",
       y = "PHE d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) + 
  geom_smooth()


ggplot(data = data, aes(Year, GLU.mean)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Glutamic Acid Shifts Through Time",
       x = "Year",
       y = "GLU d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
    geom_smooth()

ggplot(data = data, aes(Year, Trophic.Position)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Trophic Position Shifts Through Time",
       x = "Year",
       y = "Trophic Position") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_smooth()


ggplot(data = data.age3, aes(Year, PHE.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = data.age2, aes(Year, PHE.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7) 

ggplot(data = data, aes(Year, GLU.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7)


