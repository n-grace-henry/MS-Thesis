#script to compile all post processed csv files into one csv file
setwd("~/Documents/Grad School /CSIA/code/processed")

library(dplyr)
library(readr)

#compile all the csv files to make one dataframe of all data
df <- list.files(path="~/Documents/Grad School /CSIA/code/processed") %>% 
  lapply(read_csv) %>% 
  bind_rows 

#remove the first column from the data frame 
main.all <- df[,2:12]

#remove all 5AA sample IDs from the data frame
main.data <- subset(main.all, !Sample.ID == "5AA")

#add a year column to the data frame
a <- substr(main.data$Sample.ID, 1, 2)

year <- vector(mode="character", length=49)
for(i in 1:length(a)){
  if(a[i] <= 22){
    year[i] <- paste0(20, a[i])
} else{
    year[i] <- paste0(19, a[i])
}
}

main.data$Year <- year
main.data <- main.data %>% relocate(Year, .before = ALA.mean)

#add a system column
b <- substr(main.data$Sample.ID, 4, 4)

system <- vector(mode="character", length=49)
for(i in 1:length(b)){
  if(b[i] == "W"){
    system[i] <- "Wood"
  } else if(b[i] == "K"){
    system[i] <- "Kvichak"
  }  else{
    system[i] <- "Egegik"
  }
}

main.data$System <- system
main.data <- main.data %>% relocate(System, .before = ALA.mean)

#add age column
main.data$Age <- substr(main.data$Sample.ID, 6, 6)
main.data <- main.data %>% relocate(Age, .before = ALA.mean)

#write new csv file 
file.name <- "~/Documents/Grad School /CSIA/code/main.data"
write.csv(main.data, file = file.name)
