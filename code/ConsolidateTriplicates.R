#this script should be run after drift correcting and removing outliers
#from each individual run using the DriftCorrection_outliers.R script
setwd("~/Documents/GitHub/CSIA_lab_work/data")

library(dplyr)
library(readr)

#compile all the csv files to make one dataframe of all data
df <- list.files(path=setwd("~/Documents/GitHub/CSIA_lab_work/data/outliers_removed")) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df <- df[!df$AAID == "REF",] 

#####Consolidating Triplicates#####
#samples should be run in triplicate, the mean and SD of the triplicate should be taken. Output data will include mean and SD for each sample,
#the standards, with a column for every Amino Acid
#NOTE: the SD of the standards IS NOT the standard precision, column conditioning injections should be omitted to calculated standard precision
#otherwise conditioning introduces extra variability into the data. See "CalculatingStandardPrecision.R" for a file that calculates this while 
#omitting the conditioning injections

AA<- unique(unlist(df$AAID)) #make a list of the AAs in the data

mean <- aggregate(df['adj'], by = list(df$ID1, df$AAID), mean)
mean_names <- c("Sample.ID",paste0(AA,".mean"))

meanfull<-data.frame(matrix(0, nrow = length(unique(mean$Group.1)), ncol = length(AA)+1)) #initiate a dataframe for the intercepts of the linear model
colnames(meanfull) <- mean_names

meanfull[1:length(unique(mean$Group.1)),1]<- unique(mean$Group.1)
for(i in 1:length(AA)){
  meanfull[1:length(unique(mean$Group.1)),i+1]<- mean%>%
    filter(Group.2  == AA[i])%>%
    select(adj)
}

meanfull #ALWAYS check to make sure everything looks right! small errors can break the code

sd<- aggregate(df['adj'], by = list(df$ID1, df$AAID), sd)
sd_names <- c("Sample.ID",paste0(AA,".sd"))

sdfull<-data.frame(matrix(0, nrow = length(unique(sd$Group.1)), ncol = length(AA)+1)) 
colnames(sdfull)<- sd_names

sdfull[1:length(unique(sd$Group.1)),1]<- unique(sd$Group.1)
for(i in 1:length(AA)){
  sdfull[1:length(unique(sd$Group.1)),i+1]<- sd%>%
    filter(Group.2  == AA[i])%>%
    select(adj)
}
sdfull

Corrected <- merge(meanfull,sdfull, by="Sample.ID") #this merges the columns in both the SD and mean dataframes
Corrected

#####Add Year column ####
year.2digit <- substr(Corrected$Sample.ID, 1, 2)

year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}

Corrected$Year <- year
Corrected <- Corrected %>% relocate(Year, .before = ALA.mean)

#####Add System column####
sys <- substr(Corrected$Sample.ID, 4, 4)

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

Corrected$System <- system
Corrected <- Corrected %>% relocate(System, .before = ALA.mean)

#####Add Age column####
Corrected$Age <- substr(Corrected$Sample.ID, 6, 6)
Corrected <- Corrected %>% relocate(Age, .before = ALA.mean)


#####Write new .csv file that has this clean data with no outliers####
file.name <- "final/main.clean.csv"
write.csv(Corrected, file = file.name)

