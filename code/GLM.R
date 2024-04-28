setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Load Data
data <- read.csv(file = "final/data.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")
NPGO <- read.csv(file = "Environmental/NPGO.csv")


#### Convert PDO to tidy format ####
PDO_long <- gather(PDO, Month, Value, -Year)

#sort by year
PDO_long <- arrange(PDO_long, Year)

#convert month to date
PDO_long <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))






#PDO average per year
PDO_annual <- data.frame(nrow = length(PDO$Year), ncol = 2)
names(PDO_annual) <- c("Year","PDO")
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

#NPGO average per year
NPGO_annual <- NPGO %>%
  group_by(Year) %>%
  summarise(avg = mean(NPGO))
plot(NPGO_annual, type = "l")
ts.NPGO <- ts(NPGO_annual[,2],
              start = NPGO_annual[1,1],
              end = NPGO_annual[72,1],
              frequency = 1)

#Average PHE
avg_phe <- data %>% 
  group_by(Year) %>% 
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

#### Linear Model with no lagging of covariates one year before return####

#Merge data based on year
merged_df <- merge(avg_phe, PDO_annual, by = "Year", all = TRUE) %>%
  merge(NPGO_annual, by = "Year", all = TRUE)

#Rename columns
names(merged_df) <- c("Year", "PHE", "PDO", "NPGO")

#Handle missing values if any
merged_df <- na.omit(merged_df)

#Average PHE vs annual PDO + NPGO model
model <- lm(PHE ~ PDO + NPGO, data = merged_df)
summary(model)
model.2 <- lm(PHE ~ PDO, data = merged_df)
summary(model.2)
plot(model.2)
plot(x = merged_df$PDO,
     y = merged_df$PHE)


#### Linear Model with lagged covariates ####

#Create data frames with different lags/representative time periods of PDO
#Based on return month of salmon
#Each time frame ends in July 


#### Linear Model for 6 months before return ####
#January - July of return year
#Biologically this model doesn't make a lot of sense 
PDO_six_month <- data.frame(nrow = length(PDO$Year), ncol = 2)
names(PDO_six_month) <- c("Year","PDO")
for(i in 1:length(PDO$Year)){
  PDO_six_month[i,1] <- PDO$Year[i]
  PDO_six_month[i,2] <- mean(as.numeric(PDO[i,c(2:7)]))
} 

#Linear model 
merged <- merge(avg_phe, PDO_six_month, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model2 <- lm(PHE ~ PDO, data = merged)
summary(model2)
plot(x = merged$PDO,
     y = merged$PHE)

#### LM 3: One year before return July - July ####
#July - July going back from return year, labeled as return yr
PDO_1yr_ret <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_1yr_ret) <- c("Year","PDO")
for(i in 1:length(PDO$Year)){
  PDO_1yr_ret[i,1] <- PDO$Year[i+1]
  PDO_1yr_ret[i,2] <- mean(as.numeric(PDO[i, c(8:13)])) +
    mean(as.numeric(PDO[i+1, c(2:7)]))
} 

#Linear Model
merged <- merge(avg_phe, PDO_1yr_ret, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model3 <- lm(PHE ~ PDO, data = merged)
summary(model3)
plot(x = merged$PDO,
     y = merged$PHE)

#### LM 4: 18 months average before return ####
#January of previous yr - July, labeled as return yr
PDO_18mo_ret <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_18mo_ret) <- c("Year","PDO")
for(i in 1:length(PDO$Year)){
  PDO_18mo_ret[i,1] <- PDO$Year[i+1]
  PDO_18mo_ret[i,2] <- mean(as.numeric(PDO[i, c(8:13)])) +
    mean(as.numeric(PDO[i+1, c(2:7)]))
} 


#function 

#turn wide data into tidy data
library(tidyr)

?gather()
PDO_long <- gather(PDO, Month, Value, -Year)

#sort by year
PDO_long <- arrange(PDO_long, Year)

library(lubridate)

#convert month to date
PDO_long2 <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))


#Linear Model
merged <- merge(avg_phe, PDO_1yr_ret, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model3 <- lm(PHE ~ PDO, data = merged)
summary(model3)
plot(x = merged$PDO,
     y = merged$PHE)



#18 months before return 

#2 years before return


#Guess incorporation time ~4 months? End time frames in February 

#six months so  August - February 

#one year February - February 

#18 months 

#2 years 







