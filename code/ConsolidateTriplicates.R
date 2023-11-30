setwd("~/Documents/GitHub/CSIA_lab_work/data/final")

cleaned <- read.csv(file = "all.data.csv")


#####Consolidating Triplicates#####
#samples should be run in triplicate, the mean and SD of the triplicate should be taken. Output data will include mean and SD for each sample,
#the standards, with a column for every Amino Acid
#NOTE: the SD of the standards IS NOT the standard precision, column conditioning injections should be omitted to calculated standard precision
#otherwise conditioning introduces extra variability into the data. See "CalculatingStandardPrecision.R" for a file that calculates this while 
#omitting the conditioning injections

AA<- unique(unlist(cleaned$AAID)) #make a list of the AAs in the data

mean <- aggregate(cleaned['adj'], by = list(cleaned$ID1, cleaned$AAID), mean)
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

sd<- aggregate(cleaned['adj'], by = list(cleaned$ID1, cleaned$AAID), sd)
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

file.name <- "outliers_removed.csv"
write.csv(Corrected, file = file.name)

