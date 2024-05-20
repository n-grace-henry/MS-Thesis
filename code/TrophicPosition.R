#use this script after cleaning the data and removing outliers + duplicates
#calculating trophic position 
setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

library(dplyr)
library(readr)

#read in the main data file
data <- read.csv(file="all_correct_final.csv")

#define beta and TDF values, this can be changed later if necessary 
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

### Calculating trophic position: method 1 ####

#make an empty data frame to fill with Sample.ID and trophic position 
tp <- data.frame(matrix(nrow = length(data$Sample.ID), ncol = 2))
tp <-setNames(tp, c("Sample.ID","Trophic.Position"))

#for loop to calculate trophic position and fill data frame
for(i in 1:length(data$Sample.ID)){
  tp[i,2] <- 1+ ((data$GLU.mean[i]-data$PHE.mean[i]-beta)/TDF)
  tp[i,1] <- data$Sample.ID[i]
}

#combine new data frame with original 
main.trophic <- cbind(data, tp)
main.trophic <- main.trophic[, 3:18]

#create .csv file of trophic position data frame 
file.name <- "~/Documents/GitHub/CSIA_lab_work/data/final/main.trophic.csv"
write.csv(main.trophic, file = file.name)


#### Calculating trophic position: method 2 ####
# Average PHE for three time periods (periods based on plot)
period1 <- data[data$Year >= 1965 & data$Year <= 1982,]
period2 <- data[data$Year >= 1983 & data$Year <= 2005,]
period3 <- data[data$Year >= 2006 & data$Year <= 2022,]

# average PHE for each period
avg_phe1 <- period1$PHE.mean %>% mean(na.rm = TRUE)
avg_phe2 <- period2$PHE.mean %>% mean(na.rm = TRUE)
avg_phe3 <- period3$PHE.mean %>% mean(na.rm = TRUE)


# Average GLU for three time periods 


# Calculating TP from averaged PHE


# Calculating TP from GAM modeled PHE 





