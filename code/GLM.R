setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)

#Load Data
data <- read.csv(file = "final/data.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")
NPGO <- read.csv(file = "Environmental/NPGO.csv")

#Get per year average of both data frames 
library(dplyr)
PDO_annual <- PDO %>%
  group_by(Year) %>%
  summarise(avg_PHE = mean(PHE.mean, na.rm = TRUE))