#script to compile all post processed csv files into one csv file
#use this after the DriftCorrection.R script
setwd("~/Documents/GitHub/CSIA_lab_work/data/DriftCorrected_noMean")

library(dplyr)
library(readr)

#compile all the csv files to make one dataframe of all data
all.df <- list.files(path="~/Documents/GitHub/CSIA_lab_work/data/DriftCorrected_noMean") %>% 
  lapply(read_csv) %>% 
  bind_rows 

#remove all 5AA sample IDs from the data frame
all.df <- subset(all.df, !Sample.ID == "5AA")
