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
months <- 12

#must have PDO and PDO_long loaded in global environment
PDO_lag <- function(ret_yr, ret_mo, lag, months, yrs, name){
  #create empty data frame to fill with values
  PDO_new <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
  names(PDO_new) <- c("Year","PDO")
  
  #loop to get applicable values for each year
  for(i in 1:length(PDO$Year)){
    PDO_new[i,1] <- PDO$Year[i+1]
    PDO_new[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+yrs]) & 
                                    (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= ret_mo |
                                       PDO_long$Year == PDO$Year[i+yrs] & PDO_long$month_number <= ret_mo), "Value"])
    
  }
  name <- PDO_new
  return(name)
}

#everything between two points
PDO_long_subset <- PDO_long[PDO_long$Year >= 1952 & 
                              (PDO_long$Year < 1955 | (PDO_long$Year == 1955 & PDO_long$month_number <= 3)), ]


#one year of data from July-July
#if return year is 1914, and we want one yr from return, then we go back to July 1913
PDO_new <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_new) <- c("Year","PDO")
for(i in 1:length(PDO$Year)){
  PDO_new[i,1] <- PDO$Year[i+1]
  PDO_new[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1]) & 
                                  (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 7 |
                                     PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 7), "Value"])
  
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




