#this is an r script to do some basic data visualization of the cleaned data
#at this point the data should be drift corrected, compiled, and had all
#duplicates and replicates removed and averaged 
rm(list =ls())
setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

#read in the main data file
data <- read.csv(file="main.trophic.csv")

#scatter plot of year vs isotope signature
PHE.all <- ggplot(data = data, aes(Year, PHE.mean, color = System)) +
       geom_point(size = 3, alpha = 0.7)

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


#main graphs with trend line
PHE.all <- ggplot(data = data, aes(x = Year, y = PHE.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Phenylalanine (source) Signature Through Time",
       x = "Year",
       y = "PHE d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

PHE.W <- ggplot(data = Wood, aes(x = Year, y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7, color = "#619CFF") +
  labs(title = "Wood",
       x = "Year",
       y = "PHE d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

PHE.K <- ggplot(data = Kvichak, aes(x = Year, y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7, color = "#00BA38") +
  labs(title = "Kvichak",
       x = "Year",
       y = "PHE d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

PHE.E <- ggplot(data = Egegik, aes(x = Year, y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7, color = "#F8766D") +
  labs(title = "Egegik",
       x = "Year",
       y = "PHE d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

ggarrange(PHE.all, PHE.W, PHE.K, PHE.E)

GLU.all <- ggplot(data = data, aes(x = Year, y = GLU.mean, color = System)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Glutamic Acid (trophic) Signature Through Time",
       x = "Year",
       y = "GLU d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=2005, linetype ="dashed") +
  geom_smooth(aes(group=1))

GLU.W <- ggplot(data = Wood, aes(x = Year, y = GLU.mean)) +
  geom_point(size = 3, alpha = 0.7, color = "#619CFF") +
  labs(title = "Wood",
       x = "Year",
       y = "GLU d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

GLU.K <- ggplot(data = Kvichak, aes(x = Year, y = GLU.mean)) +
  geom_point(size = 3, alpha = 0.7, color = "#00BA38") +
  labs(title = "Kvichak",
       x = "Year",
       y = "GLU d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

GLU.E <- ggplot(data = Egegik, aes(x = Year, y = GLU.mean)) +
  geom_point(size = 3, alpha = 0.7, color = "#F8766D") +
  labs(title = "Egegik",
       x = "Year",
       y = "GLU d15N") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1)) 

ggarrange(GLU.all, GLU.W, GLU.K, GLU.E)

trophic.all <- ggplot(data = data, aes(x = Year, y = Trophic.Position, color = System)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Trophic Position",
       x = "Year",
       y = "Trophic Position") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1))

trophic.W <- ggplot(data = Wood, aes(x = Year, y = Trophic.Position)) +
  geom_point(size = 3, alpha = 0.7, color = "#619CFF") +
  labs(title = "Wood",
       x = "Year",
       y = "Trophic Position") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1))

trophic.K <- ggplot(data = Kvichak, aes(x = Year, y = Trophic.Position)) +
  geom_point(size = 3, alpha = 0.7, color = "#00BA38") +
  labs(title = "Kvichak",
       x = "Year",
       y = "Trophic Position") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1))

trophic.E <- ggplot(data = Egegik, aes(x = Year, y = Trophic.Position)) +
  geom_point(size = 3, alpha = 0.7, color = "#F8766D") +
  labs(title = "Egegik",
       x = "Year",
       y = "Trophic Position") +
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size=16)) +
  geom_vline(xintercept=1977, linetype ="dashed") +
  geom_vline(xintercept=1998, linetype ="dashed") +
  geom_smooth(aes(group=1))

ggarrange(trophic.all, trophic.W, trophic.K, trophic.E)

#age class histogram
ggplot(data=data, aes(x = Trophic.Position, fill = as.factor(Age), color = as.factor(Age))) +
  geom_histogram(bins = 60, position = "identity") +
  geom_density(aes(fill = as.factor(Age), color = as.factor(Age)), alpha = 0.5) +
  scale_alpha_manual(values = c(0.2, 0.5)) +
  labs(title = "Trophic Position at Ocean Age Class",
       x = "Trophic Position",
       y = "Frequency",
       fill = "Ocean Age") +
  guides(color = FALSE)

#trophic position across systems histogram
ggplot(data=data, aes(x = Trophic.Position, fill = System, color = System)) +
  geom_histogram(bins = 60, alpha = 0.5, position = "identity") +
  labs(title = "Trophic Position across system",
       x = "Trophic Position",
       y = "Frequency",
       fill = "System") +
  guides(color = FALSE)
                 
                 