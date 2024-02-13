#script to compile all post processed csv files into one csv file
#use this after the DriftCorrect_noMean.R script
setwd("~/Documents/GitHub/CSIA_lab_work/data/DriftCorrected_noMean")
rm(list)

library(dplyr)
library(readr)

#compile all the csv files to make one dataframe of all data
all.df <- list.files(path="~/Documents/GitHub/CSIA_lab_work/data/DriftCorrected_noMean") %>% 
  lapply(read_csv) %>% 
  bind_rows 

#remove all 5AA sample IDs from the data frame
all.data <- subset(all.df, !ID1 == "5AA")

#split this large data frame into 5 data frames sorted by amino acid
ALA <- subset(all.data, AAID == "ALA")
VAL <- subset(all.data, AAID == "VAL")
NOR <- subset(all.data, AAID == "NOR")
GLU <- subset(all.data, AAID == "GLU")
PHE <- subset(all.data, AAID == "PHE")

#look through data and remove outliers visually
#find the differences between the three injections (triplicates)
#if any value is 3 or more times higher than the other differences then it counts as an outlier
#for samples that have duplicates, I will only be looking at the triplicates from the same run
#for example, if 10_W_2 was run on two different days
#I will treat each set of three as there own and not compare differences over the 6 injections

for(i in 1:(length(PHE$adj)/3)){
  min(abs(PHE$adj[i]-PHE$adj[i+1]),
      abs(PHE$adj[i+1]-PHE$adj[i+2]),
      abs(PHE$adj[i]-PHE$adj[i+2])) * 3 <= max(abs(PHE$adj[i]-PHE$adj[i+1]),
                                               abs(PHE$adj[i+1]-PHE$adj[i+2]),
                                               abs(PHE$adj[i]-PHE$adj[i+2]))
  
}

#changing the format of the data to make visualization of each sample easier
samples <- vector(length = 79)
for(i in 0:78){
  samples[i+1] <- PHE$ID1[(i*3)+1]
}

wide.format <- data.frame(matrix(nrow=4, ncol=79))
for(i in 1:79){
  wide.format[1,i] <- samples[i]
}

for(i in 0:78){
  wide.format[2,i+1] <- PHE$adj[(i*3)+1]
  wide.format[3,i+1] <- PHE$adj[(i*3)+2]
  wide.format[4,i+1] <- PHE$adj[(i*3)+3]
}

#identify which samples have outlier values 
for(i in 1:79)){
  min(abs(PHE$adj[i]-PHE$adj[i+1]),
      abs(PHE$adj[i+1]-PHE$adj[i+2]),
      abs(PHE$adj[i]-PHE$adj[i+2])) * 3 <= max(abs(PHE$adj[i]-PHE$adj[i+1]),
                                               abs(PHE$adj[i+1]-PHE$adj[i+2]),
                                               abs(PHE$adj[i]-PHE$adj[i+2]))
  
}





