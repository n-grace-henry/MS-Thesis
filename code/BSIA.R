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

#get rid of standards 
STD <- c("NOR", "ALA", "VAL", "PHE", "GLU")

index <- which(substr(combined_df$Identifier.1, 1, 3) == "NOR" | 
         substr(combined_df$Identifier.1, 1, 3) == "ALA" | 
         substr(combined_df$Identifier.1, 1, 3) == "VAL" | 
         substr(combined_df$Identifier.1, 1, 3) == "PHE" | 
         substr(combined_df$Identifier.1, 1, 3) == "GLU")

no_std <- combined_df[-index,]

#add year column
year.2digit <- substr(no_std$Identifier.1, 1, 2)

year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}

no_std$Year <- year
no_std <- no_std %>% relocate(Year, .before = Row)

#add age column
no_std$Age <- substr(no_std$Identifier.1, 7, 7)
no_std <- no_std %>% relocate(Age, .before = Row)

#plot
ggplot(data = no_std,
       aes(x = Year, y = d.15N.14N.air, color = Age)) +
  geom_point(size = 3, alpha = 0.7) 

#group samples 
grouped_df <- combined_df %>%
  group_by(group_id = substr(Identifier.1, 1, 7)) %>%
  summarize_all(mean, na.rm = TRUE)

#get rid of extra columns
grouped_df <- grouped_df[,-c(2:7)]

#remove AA standards and make own sheet 
AA_STDS <- grouped_df[22:71,]

grouped_AAs <- AA_STDS %>%
  group_by(ID = substr(group_id, 1, 3)) %>%
  summarize_all(mean, na.rm = TRUE)

#make sheet of no AAs
sample_df <- grouped_df[1:21,]

##### Add Year Column ####
year.2digit <- substr(sample_df$group_id, 1, 2) #none of this is updated, only copy pasted

year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}

sample_df$Year <- year
sample_df <- sample_df %>% relocate(Year, .before = group_id)

##### Add Age Column ####
sample_df$Age <- substr(sample_df$group_id, 7, 7)
sample_df <- sample_df %>% relocate(Age, .before = group_id)

#plot
library(ggplot2)
plot(x = sample_df$d.13C.12C.VPDB,
     y = sample_df$d.15N.14N.air,
     xlab = "d13C",
     ylab = "d15N",
     main = "d13C vs d15N")
# year vs d13C and see if I get a trend



