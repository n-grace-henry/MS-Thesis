#code to average all the duplicate samples and add the new average to the main
#data file
rm(list = ls())
setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

library(dplyr)
library(readr)

data <- read.csv(file="main.clean.csv")

#take a look at all replicate values
#make a new column with only the first chunk of the sample ID
#after this they can be removed 
data$new.ID <- substr(data$Sample.ID, 1, 6)

rep <- substr(data$Sample.ID, 8, 8)
data$rep <- substr(data$Sample.ID, 8, 8)

which(data$rep == "R")

data[3,]
E013 <- subset(data, new.ID == "01_E_3")

data[9,]
W042<- subset(data, new.ID == "04_E_2")

data[22,]
E102 <- subset(data, new.ID == "10_W_2")

data[35,]
K222 <- subset(data, new.ID == "22_E_2")

data[40,]
K223 <- subset(data, new.ID == "22_K_3")

data[58,]
K742 <- subset(data, new.ID == "74_W_2")

data[66,]
K892 <- subset(data, new.ID == "89_K_2")

df <- rbind(E013,W042,E102,K222,K223,K742,K892)

#write a new .csv file for the replicates to look at later if needed
file.name <- "~/Documents/GitHub/CSIA_lab_work/data/final/replicates.csv"
write.csv(df, file = file.name)

#find mean difference of replicates later using this saved .csv file
#from calculator, mean difference right now equals 1.876 (not updated as of 02/14)

#function to average duplicate/replicates and replace in data file with new averages
#run this function as many times as replicates there are
rm_duplicates <- function(df, ID, Year, System, Age){
  a <- subset(df, new.ID == ID)
  b <- a[,6:15] #adjust this based on size of data being edited
  vec <- vector(mode="numeric", length=10)
  
  for(i in 1:10){
    vec[i] <- mean(as.numeric(b[,i]))
  }
  
  c <- append(c(1, ID, Year, System, Age),c(vec, ID, 0))
  norep <- df[!df$new.ID==ID,]
  new.data <- rbind(norep, c)
  
  print(new.data)
}

data <- rm_duplicates(df = data, ID = "01_E_3", Year = "2001", System = "Egegik", Age = "3")
data <- rm_duplicates(df = data, ID = "22_K_3", Year = "2022", System = "Kvichak", Age = "3")
data <- rm_duplicates(df = data, ID = "22_W_3", Year = "2022", System = "Wood", Age = "3")
data <- rm_duplicates(df = data, ID = "13_W_2", Year = "2013", System = "Wood", Age = "2")
data <- rm_duplicates(df = data, ID = "13_W_3", Year = "2013", System = "Wood", Age = "3")
data <- rm_duplicates(df = data, ID = "04_E_2", Year = "2004", System = "Egegik", Age = "2")
data <- rm_duplicates(df = data, ID = "10_W_2", Year = "2010", System = "Wood", Age = "2")
data <- rm_duplicates(df = data, ID = "22_E_2", Year = "2022", System = "Egegik", Age = "2")
data <- rm_duplicates(df = data, ID = "74_W_2", Year = "1974", System = "Wood", Age = "2")
data <- rm_duplicates(df = data, ID = "89_K_2", Year = "1989", System = "Kvichak", Age = "2")
data <- rm_duplicates(df = data, ID = "89_W_2", Year = "1989", System = "Wood", Age = "2")

#check to see if there are any duplicate samples left 
anyDuplicated(df$new.ID)

#remove the last two columns
data <- df
data <- as.data.frame(data[,1:9])


#### Trophic Position Calculations ####

#define beta and TDF values, this can be changed later if necessary 
beta <- 3.4 #commonly used constant
TDF <- 7.06 #from Lerner et al 2020

#make an empty data frame to fill with Sample.ID and trophic position 
tp <- data.frame(matrix(nrow = length(data$Sample.ID), ncol = 2))
tp <-setNames(tp, c("Sample.ID","Trophic.Position"))

#for loop to calculate trophic position and fill data frame
for(i in 1:length(data$Sample.ID)){
  tp[i,2] <- 1 + ((as.numeric(data$GLU.mean[i])-as.numeric(data$PHE.mean[i])-beta)/TDF)
  tp[i,1] <- data$Sample.ID[i]
}

#combine new data frame with original 
data <- cbind(data, tp)
data <- data[, 2:11]
data <- data[,-9]

#write new file
file.name <- "final/all_correct_final.csv"
write.csv(data, file.name)








