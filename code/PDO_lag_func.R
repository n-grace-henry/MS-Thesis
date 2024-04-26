setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Load Data
PDO <- read.csv(file = "Environmental/PDO.csv")

#### Convert to tidy format ####
PDO_long <- gather(PDO, Month, Value, -Year)

#sort by year
PDO_long <- arrange(PDO_long, Year)

#convert month to date
PDO_long <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))


#### Function ####

ret_yr <- 1960
ret_mo <- 7
lag <- 0
months <- 18

PDO_new <- data.frame(nrow = length(PDO$Year), ncol = 2)
names(PDO_new) <- c("Year","PDO")
for(i in 1:length(PDO$Year)){
  PDO_six_month[i,1] <- PDO$Year[i]
  PDO_six_month[i,2] <- mean(as.numeric(PDO[i,c(2:7)]))
} 



#NPGO for reference
NPGO_annual <- NPGO %>%
  group_by(Year) %>%
  summarise(avg = mean(NPGO))
plot(NPGO_annual, type = "l")
ts.NPGO <- ts(NPGO_annual[,2],
              start = NPGO_annual[1,1],
              end = NPGO_annual[72,1],
              frequency = 1)




