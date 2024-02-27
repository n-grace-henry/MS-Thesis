#Run this file after running DriftCorrection.R, which corrects data
#but does not remove any outliers. This script will take the folder 
#named "with_outliers" that is produced in the previous script, and
#compiles those scripts to remove duplicates. 
#This script also calculates trophic position and adds a TP column to 
#the dataset. The output of this script is a file called "all.data.csv"
#which can be found in the "final" folder.
setwd("~/Documents/GitHub/CSIA_lab_work/data")
rm(list = ls())

library(dplyr)
library(readr)

#compile all the csv files to make one dataframe of all data
df <- list.files(path=setwd("~/Documents/GitHub/CSIA_lab_work/data/with_outliers")) %>% 
  lapply(read_csv) %>% 
  bind_rows 

#Remove 5AA and REF columns
df <- df[!df$Sample.ID == "5AA",]
df <- df[,1:12]

#####Add Year column ####
year.2digit <- substr(df$Sample.ID, 1, 2)

year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}

df$Year <- year
df <- df %>% relocate(Year, .before = VAL.mean)

#####Add System column####
sys <- substr(df$Sample.ID, 4, 4)

system <- vector(mode="character")
for(i in 1:length(sys)){
  if(sys[i] == "W"){
    system[i] <- "Wood"
  } else if(sys[i] == "K"){
    system[i] <- "Kvichak"
  }  else{
    system[i] <- "Egegik"
  }
}

df$System <- system
df <- df %>% relocate(System, .before = VAL.mean)

#####Add Age column####
df$Age <- substr(df$Sample.ID, 6, 6)
df <- df %>% relocate(Age, .before = VAL.mean)

####Add column of replicates####
df$new.ID <- substr(df$Sample.ID, 1, 6) #new.ID gets rid of R in sample.ID

rep <- substr(data$Sample.ID, 8, 8)
data$rep <- substr(data$Sample.ID, 8, 8)

#function to average duplicate/replicates and replace in data file with new averages
#run this function as many times as replicates there are
rm_duplicates <- function(df, ID, Year, System, Age){
  a <- subset(df, new.ID == ID)
  b <- a[,6:15]
  vec <- vector(mode="numeric", length=10)
  
  for(i in 1:10){
    vec[i] <- mean(as.numeric(b[,i]))
  }
  
  c <- append(c(1, ID, Year, System, Age),c(vec, ID, 0))
  norep <- df[!df$new.ID==ID,]
  new.data <- rbind(norep, c)
  
  print(new.data)
}

data <- rm_duplicates(df = df, ID = "01_E_3", Year = "2001", System = "Egegik", Age = "3")
data <- rm_duplicates(df = df, ID = "22_K_3", Year = "2022", System = "Kvichak", Age = "3")
data <- rm_duplicates(df = df, ID = "22_W_3", Year = "2022", System = "Wood", Age = "3")
data <- rm_duplicates(df = df, ID = "13_W_2", Year = "2013", System = "Wood", Age = "2")
data <- rm_duplicates(df = df, ID = "13_W_3", Year = "2013", System = "Wood", Age = "3")
data <- rm_duplicates(df = df, ID = "04_E_2", Year = "2004", System = "Egegik", Age = "2")
data <- rm_duplicates(df = df, ID = "10_W_2", Year = "2010", System = "Wood", Age = "2")
data <- rm_duplicates(df = df, ID = "22_E_2", Year = "2022", System = "Egegik", Age = "2")
data <- rm_duplicates(df = df, ID = "74_W_2", Year = "1974", System = "Wood", Age = "2")
data <- rm_duplicates(df = df, ID = "89_K_2", Year = "1989", System = "Kvichak", Age = "2")
data <- rm_duplicates(df = df, ID = "89_W_2", Year = "1989", System = "Wood", Age = "2")


#check to see if there are any duplicate samples left 
anyDuplicated(data$new.ID)

#remove the last two columns
main.data <- data
main.data <- main.data[,1:15]

#place holder so I can look at this data before I figure out these reps
file.name <- "~/Documents/GitHub/CSIA_lab_work/data/final/main.data.csv"
write.csv(main.data, fi

