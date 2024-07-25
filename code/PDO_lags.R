setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Load Data
data <- read.csv(file = "final/all_correct_final.csv")
PDO <- read.csv(file = "Environmental/PDO.csv")

#### Convert PDO to tidy format ####
PDO_long <- gather(PDO, Month, Value, -Year)

#sort by year
PDO_long <- arrange(PDO_long, Year)

#convert month to date
PDO_long <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))

#plot PDO from 1965-2022
PDO_subset <- subset(PDO_long, Year >= 1965 & Year <= 2022)
PDO_annual <- PDO_subset %>%
  group_by(Year) %>%
  summarise(Value = mean(Value, na.rm = TRUE))

plot(x = PDO_annual$Year,
     y = PDO_annual$Value,
     type = "l",
     xlab = "Year",
     ylab = "PDO")

#### PDO data formatting per year ####
# 2 years of PDO averaged before return year
PDO_avg <- data.frame(nrow = length(PDO$Year), ncol = 2)
names(PDO_avg) <- c("Year","PDO")





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



