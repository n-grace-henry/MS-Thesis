setwd("~/Documents/GitHub/CSIA_lab_work/data/SIA results")

#Code to take raw data referenced to nitrogen tank and convert to the 
#VPDB international standard
#Looking at three separate runs and will be comparing corrections found 
#across runs



### First EA run ###
#Load in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/SIA results/230914_Jameson_10B-AqPlants.csv")

#True values referenced to VPDB
GA1 <- 
GA2 <- 
PCH <- 

#Grab all GA1 data
GA1 <- data[data$Identifier.1 %in% c("GA1_QTY_2", 
                                     "GA1_STD_01", 
                                     "GA1_STD_02", 
                                     "GA1_STD_03", 
                                     "GA1_STD_04"), ]

#Grab all the GA2 data
GA2 <- data[data$Identifier.1 %in% c("GA2_QTY_02", 
                                     "GA2_STD_01", 
                                     "GA2_STD_02", 
                                     "GA2_STD_03", 
                                     "GA2_STD_04"), ]

#Grab all the SAL data
PCH <- data[data$Identifier.1 %in% c("PCH_STD_01", 
                                     "PCH_STD_02", 
                                     "PCH_STD_03", 
                                     "PCH_STD_04"), ]



