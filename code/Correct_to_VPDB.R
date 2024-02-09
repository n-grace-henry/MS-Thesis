setwd("~/Documents/GitHub/CSIA_lab_work/data/SIA results")

#Code to take raw data referenced to nitrogen tank and convert to the 
#air international standard
#Looking at three separate runs and will be comparing corrections found 
#across runs
#Performing all calculations in R notation and converting to delta 
#notation as final step 



### First EA run ###
#Load in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/SIA results/230914_Jameson_10B-AqPlants.csv")

#True values referenced to air
GA1.ref <- -4.6
GA2.ref <- -5.7
PCH.ref <- 1.98
SAL.ref <- 11.3

#Grab all GA1 data
GA1 <- data[data$Identifier.1 %in% c("GA1_QTY_2", 
                                     "GA1_STD_01", 
                                     "GA1_STD_02", 
                                     "GA1_STD_03", 
                                     "GA1_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]
GA1.avg <- mean(GA1)

#Grab all the GA2 data
GA2 <- data[data$Identifier.1 %in% c("GA2_QTY_02", 
                                     "GA2_STD_01", 
                                     "GA2_STD_02", 
                                     "GA2_STD_03", 
                                     "GA2_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]

GA2.avg <- mean(GA2)

#Grab all the SAL data
PCH <- data[data$Identifier.1 %in% c("PCH_STD_01", 
                                     "PCH_STD_02", 
                                     "PCH_STD_03", 
                                     "PCH_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]

PCH.avg <- mean(PCH)

#Find offset values
GA1.off <- GA1.avg.R - GA1.r 
GA2.off <- GA2.avg.R - GA2.r
PCH.off <- PCH.avg.R - PCH.r


