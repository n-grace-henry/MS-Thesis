setwd("~/Documents/GitHub/CSIA_lab_work/data/SIA results")

#Code to take raw data referenced to nitrogen tank and convert to the 
#air international standard
#Looking at three separate runs and will be comparing corrections found 
#across runs
#Performing all calculations in R notation and converting to delta 
#notation as final step 

#True values referenced to air
GA1.ref <- -4.6
GA2.ref <- -5.7
PCH.ref <- 1.98
SAL.ref <- 11.3

####### Data Set One #######
#Load in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/SIA results/230914_Jameson_10B-AqPlants.csv")

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
GA1.off <- GA1.avg- GA1.ref
GA2.off <- GA2.avg - GA2.ref
PCH.off <- PCH.avg - PCH.ref

#Plot 
offset.1 <- c(GA1.off, GA2.off, PCH.off)
measured.1 <- c(GA1.avg, GA2.avg, PCH.avg)

plot(x = measured.1, y = offset.1)

#Linear regression
lm.1 <- lm(offset.1 ~ measured.1)
summary(lm.1)


####### Data Set Two #######
#Load in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/SIA results/231001_Jameson_20A-Snails.csv")

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
SAL <- data[data$Identifier.1 %in% c("SAL_STD_01", 
                                     "SAL_STD_02", 
                                     "SAL_STD_03", 
                                     "SAL_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]

SAL.avg <- mean(SAL)

#Find offset values
GA1.off <- GA1.avg - GA1.ref
GA2.off <- GA2.avg - GA2.ref
SAL.off <- SAL.avg - SAL.ref

#Plot 
offset.2 <- c(GA1.off, GA2.off, SAL.off)
measured.2 <- c(GA1.avg, GA2.avg, SAL.avg)

plot(x = measured.2, y = offset.2)

#Linear regression
lm.2 <- lm(offset.2 ~ measured.2)
summary(lm.2)


####### Data Set Three #######
#Load in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/SIA results/231109_DoranEtOH4.csv")

#Grab all GA1 data
GA1 <- data[data$Identifier.1 %in% c("GA1_QTY_", 
                                     "GA1_STD_01", 
                                     "GA1_STD_02", 
                                     "GA1_STD_03", 
                                     "GA1_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]

#"d.15N.14N"
GA1.avg <- mean(GA1[1:6])

#Grab all the GA2 data
GA2 <- data[data$Identifier.1 %in% c("GA2_QTY_", 
                                     "GA2_STD_01", 
                                     "GA2_STD_02", 
                                     "GA2_STD_03", 
                                     "GA2_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]

GA2.avg <- mean(GA2)

#Grab all the SAL data
SAL <- data[data$Identifier.1 %in% c("SAL_STD_01", 
                                     "SAL_STD_02", 
                                     "SAL_STD_03", 
                                     "SAL_STD_04") &
              data$Is.Ref._ == "0" &
              data$Gasconfiguration == "N2", "d.15N.14N"]

SAL.avg <- mean(SAL)

#Find offset values
GA1.off <- GA1.avg- GA1.ref
GA2.off <- GA2.avg - GA2.ref
SAL.off <- SAL.avg - SAL.ref

#Plot 
offset.3 <- c(GA1.off, GA2.off, SAL.off)
measured.3 <- c(GA1.avg, GA2.avg, SAL.avg)

plot(x = measured.3, y = offset.3)

#Linear regression
lm.3 <- lm(offset.3 ~ measured.3)
summary(lm.3)

