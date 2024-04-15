#processing the bulk data using the HEEL package
devtools::install_github("gholtgrieve/HEEL")
rm(list=ls())
library(HEEL)

#run this code for all csv files in the "cleaned" folder
HEEL::EA.NACHO("~/Documents/GitHub/CSIA_lab_work/data/EA results/cleaned/240410_GHenry_20240410.csv")
both

#extract data from all csv files 
setwd("~/Documents/GitHub/CSIA_lab_work/data/EA results/processed")
apr10 <- read.csv(file = "04:10:2024/sample_CN.csv")
apr08 <- read.csv(file = "04:08:2024/sample_CN.csv")
apr03 <- read.csv(file = "04:03:2024/sample_CN.csv")
mar29 <- read.csv(file = "03:29:2024/sample_CN.csv")
mar28 <- read.csv(file = "03:28:2024/sample_CN.csv")
mar27 <- read.csv(file = "03:27:2024/sample_CN.csv")
mar26 <- read.csv(file = "03:26:2024/sample_CN.csv")
mar25 <- read.csv(file = "03:25:2024/sample_CN.csv")

#combine into one sheet
library(dplyr)
combined_df <- bind_rows(apr03, apr08, mar25, mar26, mar27, mar28, mar29)

#group samples 
grouped_df <- combined_df %>%
  group_by(group_id = substr(Identifier.1, 1, 7)) %>%
  summarize_all(mean, na.rm = TRUE)

#get rid of extra columns
grouped_df <- grouped_df[,-c(2:7)]

#remove AA standards and make own sheet 
AA_STDS <- 

#plot
library(ggplot2)
plot(grouped_df$d.13C.12C.VPDB)




