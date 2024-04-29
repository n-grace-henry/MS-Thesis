setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Load Data
data <- read.csv(file = "final/data.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")

#### Convert PDO to tidy format ####
PDO_long <- gather(PDO, Month, Value, -Year)

#sort by year
PDO_long <- arrange(PDO_long, Year)

#convert month to date
PDO_long <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))

#### Average PHE ###
avg_phe <- data %>% 
  group_by(Year) %>% 
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

#### Linear Model with no lagging of covariates one year before return ####

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
PDO_1yr_ret <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_1yr_ret) <- c("Year","PDO")

for(i in 1:length(PDO$Year)){
  PDO_1yr_ret[i,1] <- PDO$Year[i+1]
  PDO_1yr_ret[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1]) & 
                                    (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 7 |
                                     PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 7), "Value"])
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
  PDO_18mo_ret[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1]) & 
                                       (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 1 |
                                          PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 7), "Value"])
} 

#Linear Model 4
merged <- merge(avg_phe, PDO_18mo_ret, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model4 <- lm(PHE ~ PDO, data = merged)
summary(model4)
plot(model4)
plot(x = merged$PDO,
     y = merged$PHE)

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")


#2 years before return LM 5

#### LM 6: 18 months before return with 6 month lag ####
PDO_18mo_lag <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_18mo_lag) <- c("Year","PDO")

for(i in 1:length(PDO$Year)){
  PDO_18mo_lag[i,1] <- PDO$Year[i+2]
  PDO_18mo_lag[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1], PDO$Year[i+2]) & 
                                       (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 7 |
                                          PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 12 |
                                          PDO_long$Year == PDO$Year[i+2]& PDO_long$month_number <= 1), "Value"])
} 
#Linear Model 6
merged <- merge(avg_phe, PDO_18mo_lag, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model6 <- lm(PHE ~ PDO, data = merged)
summary(model6)
plot(model6)

plot(x = merged$PDO,
     y = merged$PHE)

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")


#### LM 7: 2 years before return with 6 month lag ####
PDO_2yr_lag <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_2yr_lag) <- c("Year","PDO")

for(i in 1:length(PDO$Year)){
  PDO_2yr_lag[i,1] <- PDO$Year[i+2]
  PDO_2yr_lag[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1], PDO$Year[i+2]) & 
                                       (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 2 |
                                          PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 12 |
                                          PDO_long$Year == PDO$Year[i+2]& PDO_long$month_number <= 1), "Value"])
} 
#Linear Model 7
merged <- merge(avg_phe, PDO_2yr_lag, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model7 <- lm(PHE ~ PDO, data = merged)
summary(model7)
plot(model7)

plot(x = merged$PDO,
     y = merged$PHE)

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")


#### LM 8: 3 years before return 6 month lag ####
PDO_3yr_lag <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_3yr_lag) <- c("Year","PDO")

for(i in 1:length(PDO$Year)){
  PDO_3yr_lag[i,1] <- PDO$Year[i+2]
  PDO_3yr_lag[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1], PDO$Year[i+2]) & 
                                      (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 2 |
                                         PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 12 |
                                         PDO_long$Year == PDO$Year[i+2]& PDO_long$month_number <= 1), "Value"])
} 


i <- 1
PDO$Year[i+3]
a <- PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1], PDO$Year[i+2],PDO$Year[i+3]) & 
                                    (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 1 |
                                       PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 12 |
                                       PDO_long$Year == PDO$Year[i+2]& PDO_long$month_number <= 12|
                                       PDO_long$Year == PDO$Year[i+3]& PDO_long$month_number <= 1), ]


