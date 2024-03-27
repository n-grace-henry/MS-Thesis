setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)

#Load Data
data <- read.csv(file = "final/data.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")
NPGO <- read.csv(file = "Environmental/NPGO.csv")

#PDO average per year
PDO_annual <- matrix(nrow = length(PDO$Year), ncol = 2)
for(i in 1:length(PDO$Year)){
  PDO_annual[i,1] <- PDO$Year[i]
  PDO_annual[i,2] <- mean(as.numeric(PDO[i, 2:13]))
}
plot(PDO_annual,
     type = "l")
ts.PDO <- ts(PDO_annual[,2], 
             start = PDO_annual[1,1],
             end = PDO_annual[169,1],
             frequency = 1)
plot(x = ts.PDO)
cpt <- cpt.mean(as.vector(scale(ts.PDO)), method = "PELT")
plot(cpt, cpt.col = "blue")
summary(cpt)

#NPGO average per year
NPGO_annual <- NPGO %>%
  group_by(Year) %>%
  summarise(avg = mean(NPGO))
plot(NPGO_annual, type = "l")
ts.NPGO <- ts(NPGO_annual[,2],
              start = NPGO_annual[1,1],
              end = NPGO_annual[72,1],
              frequency = 1)
cpt.N <- cpt.mean(as.vector(scale(ts.NPGO)), method = "PELT")
plot(cpt.N, cpt.col ="blue")

