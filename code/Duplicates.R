oct12 <- read.csv(file = "20231012_corrected.csv")
aug29 <- read.csv(file= "20230829_corrected.csv")
sep24 <- read.csv(file= "20230924_corrected.csv")
sep22 <- read.csv(file="20230922_corrected.csv")

"04-E-2-R" <- oct12[1,"PHE.mean"]
"04-E-2" <- sep22[1,"PHE.mean"]

"10-W-2-R" <- oct12[2,"PHE.mean"]
"10-W-2" <- sep24[3, "PHE.mean"]

"22-K-3-R" <- oct12[3, "PHE.mean"]
"22-K-3" <- aug29[1, "PHE.mean"]

oct12[1,"GLU.mean"]
sep22[1,"GLU.mean"]

oct12[2,"GLU.mean"]
sep24[3, "GLU.mean"]

oct12[3, "GLU.mean"]
aug29[1, "GLU.mean"]

#code to average all the duplicate samples and add the new average to the main
#data file. This starts with a csv file produced by the Compile.R script. 
setwd("~/Documents/Grad School /CSIA/code")

library(dplyr)
library(readr)

data <- read.csv(file="main.data")

#function to average duplicates and replace in data file with new averages
#this function works only when samples are not replicates (difference is 
#that these samples were the same sample, injected more than once)
rm_duplicates <- function(df, ID, Year, System, Age){
  a <- subset(df, Sample.ID == ID)
  b <- a[,6:15]
  vec <- vector(mode="numeric", length=10)
  
  for(i in 1:10){
    vec[i] <- mean(as.numeric(b[,i]))
  }
  
  c <- append(c(1, ID, Year, System, Age), vec)
  norep <- df[!df$Sample.ID==ID,]
  new.df <- rbind(norep, c)
  
  print(new.df)
}

data <- rm_duplicates(df = data, ID = "22_K_3", Year = "2022", System = "Kvichak", Age = "3")
data <- rm_duplicates(df = data, ID = "22_W_3", Year = "2022", System = "Wood", Age = "3")

main.data <- data
#check to see if there are any duplicate samples left 
anyDuplicated(data$Sample.ID)

#place holder so I can look at this data before I figure out these reps
file.name <- "~/Documents/Grad School /CSIA/code/main.data"
write.csv(main.data, file = file.name)

#from calculator, mean difference right now equal 1.876

#get rid of duplicates before dealing with replicate samples
#start by removing sample ID _R which identifies replicates
rep <- substr(data$Sample.ID, 1, 6)
data$rep <- substr(data$Sample.ID, 1, 6)
which(duplicated(rep))

rep[42]
E042 <- subset(data, rep == "04_E_2")

rep[43]
W102<- subset(data, rep == "10_W_2")

rep[58]
E013 <- subset(data, rep == "01_E_3")

rep[60]
K892 <- subset(data, rep == "89_K_2")

rep[63]
K223 <- subset(data, rep == "22_K_3")

df <- rbind(E042, W102, E013, K892, K223)

