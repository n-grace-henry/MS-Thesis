rm(list = ls())

setwd("~/Documents/Grad School /CSIA")
library(dplyr)

#DO NOT MODIFY THIS CODE AND PUSH TO THE REPOSITORY

#Names of columns, to run this code you need at least "Analysis" which refers to the injection number,
#"ID1" which is the sample ID, "RT" which is the retention time, "d15N" the nitrogen stable isotope value in permille
# and "AAID" which the the amino acid identification from the GC output

name <- c("Analysis", "ID1", "RT", "AreaAll", "d29N", "d15N", "AAID") 

#This is the stable isotpe ratios of the internal and external standards. 
# MAKE SURE THIS IS UP TO DATE BASED ON HEEL STANDARDS!!! These values are as of 01/23/2020. 
#If your samples were esterified after 01/23/2020 these values should be verfied with the standard file on the HEEL drive
ALA <- -1.21
VAL <- 0.361
NOR <- 14.163
PHE <- -5.004
GLU <- -3.336

#Reading in the .csv of the NACHO data file and setting the file name for your output file
data.1 <- SL.1 <- read.csv("code/cleaned/20231118_GHenry_CSIA.csv") #modify with name of your data file
colnames(data.1)<-name
file.name <- "~/Documents/GitHub/CSIA_lab_work/data/outliers_removed/20231118_outliersRem.csv" #file name for output file including relative file path

###### Linear Model for Drift Correction #####
#Fit a linear model to your external standards with "Analysis" (injection number) as the dependent variable and 
#d15N as the response variable

data.1STD <- subset(data.1, ID1=="5AA") #get only the standard data 
AA<- unique(unlist(data.1STD$AAID)) #make a list of the AAs in the data

Intercept<-data.frame(Intercept=rep(NA,length(AA))) #initiate a dataframe for the intercepts of the linear model
for(i in 1:length(AA)){
  data <- subset(data.1STD, AAID==AA[i])
  Intercept[i,1]<- coef(summary(lm(as.numeric(d15N)~as.numeric(Analysis), data=data)))[1,1]
}
Intercept #intercept values looped by aa


Slope<-data.frame(Slope=rep(NA,length(AA))) #initiate a dataframe for the slopes of the linear model
for(i in 1:length(AA)){
  data <- subset(data.1STD, AAID==AA[i])
  Slope[i,1]<- coef(summary(lm(as.numeric(d15N)~as.numeric(Analysis), data=data)))[2,1]
}
Slope #slope values looped by aa


Coef<- data.frame(AA, Intercept, Slope) #creating a dataframe of the slope and intercepts values for each AA

##### Adding Coefs and Standard Values to the dataset####


actual <- ifelse(data.1$AAID=="NOR", NOR, 
                 ifelse(data.1$AAID=="ALA", ALA,
                        ifelse(data.1$AAID=="VAL", VAL,
                               ifelse(data.1$AAID=="PHE", PHE,
                                      ifelse(data.1$AAID=="GLU", GLU,0)))))

actual #check your data -- if there are 0s than you have an AA that is not included in the standard 12AA mix and the code will need
#to be modified accordingly. Data should only be used for AAs that are included in the external standard or standards that 
#do not have a substantial drift (check your d15N verse Analysis plot to verify if the AA is drifting)

slope <- ifelse(data.1$AAID=="NOR", filter(Coef, AA=="NOR")[1,3], 
                ifelse(data.1$AAID=="ALA", filter(Coef, AA=="ALA")[1,3],
                       ifelse(data.1$AAID=="VAL", filter(Coef, AA=="VAL")[1,3],
                              ifelse(data.1$AAID=="GLU", filter(Coef, AA=="GLU")[1,3],
                                     ifelse(data.1$AAID=="PHE", filter(Coef, AA=="PHE")[1,3], 0)))))

intercept <-   ifelse(data.1$AAID=="NOR", filter(Coef, AA=="NOR")[1,2], 
                      ifelse(data.1$AAID=="ALA", filter(Coef, AA=="ALA")[1,2],
                             ifelse(data.1$AAID=="VAL", filter(Coef, AA=="VAL")[1,2],
                                    ifelse(data.1$AAID=="GLU", filter(Coef, AA=="GLU")[1,2],
                                           ifelse(data.1$AAID=="PHE", filter(Coef, AA=="PHE")[1,2], 0)))))


#####Applying Drift Correction####
difference <- actual-(data.1$Analysis*slope+intercept) #Applying both a drift and step correction in on estep from linear model data
adj <- data.1$d15N + difference
data <- cbind(data.1, adj)

#####Remove all 5AA samples from dataset####
data <- subset(data, !ID1 == "5AA")

#####Add Year column ####
a <- substr(data$ID1, 1, 2)

year <- vector(mode="character")
for(i in 1:length(a)){
  if(a[i] <= 22){
    year[i] <- paste0(20, a[i])
  } else{
    year[i] <- paste0(19, a[i])
  }
}

data$Year <- year
data <- data %>% relocate(Year, .before = RT)

#####Add System column####
b <- substr(data$ID1, 4, 4)

system <- vector(mode="character")
for(i in 1:length(b)){
  if(b[i] == "W"){
    system[i] <- "Wood"
  } else if(b[i] == "K"){
    system[i] <- "Kvichak"
  }  else{
    system[i] <- "Egegik"
  }
}

data$System <- system
data <- data %>% relocate(System, .before = RT)

#####Add Age column####
data$Age <- substr(data$ID1, 6, 6)
data <- data %>% relocate(Age, .before = RT)

#####Removing Outliers####
#only removing outliers for PHE and GLU

sample.ID <- unique(data$ID1)
sample.ID
values <- data[data$AAID == "PHE" & data$ID1 == sample.ID[3], "adj"]
values #look at values and determine if there are outliers 

#remove the outlier row, only run this line of code when there is an outlier to remove
#data <- data[!data$adj == values[3],] 


#do the sample process with GLU
values <- data[data$AAID == "GLU" & data$ID1 == sample.ID[3], "adj"]
values #look at values and determine if there are outliers 

#remove the outlier row, only run this line of code when there is an outlier to remove
#data <- data[!data$adj == values[3],] 


#generate .csv file of this data with no outliers
write.csv(data, file = file.name)

