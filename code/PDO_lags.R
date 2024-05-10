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

#### Average PHE ###
avg_phe <- data %>% 
  group_by(Year) %>% 
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

plot(x = avg_phe$Year,
     y = avg_phe$PHE,
     xlab = "Year",
     ylab = "PHE signature")

# subset three year time periods (based on PDO)
period1 <- data[data$Year >= 1965 & data$Year <= 1976,]
period2 <- data[data$Year >= 1977 & data$Year <= 2000,]
period3 <- data[data$Year >= 1999 & data$Year <= 2022,]

# average PHE for each period
avg_phe1 <- period1$PHE.mean %>% mean(na.rm = TRUE)
avg_phe2 <- period2$PHE.mean %>% mean(na.rm = TRUE)
avg_phe3 <- period3$PHE.mean %>% mean(na.rm = TRUE)

ggplot(data = data, aes(x = Year, y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_segment(x = 1965, xend = 1976, y = avg_phe1, yend = avg_phe1, color = "red") +
  geom_segment(x = 1977, xend = 2000, y = avg_phe2, yend = avg_phe2, color = "blue") +
  geom_segment(x = 1999, xend = 2022, y = avg_phe3, yend = avg_phe3, color = "green") +
  labs(title = "Average Phenylalanine (PHE) Across Time Periods",
       x = "Year",
       y = "Average PHE") +
  theme_minimal()


# subset three year time periods (based visually where the breaks seem to be)
period1.b <- data[data$Year >= 1965 & data$Year <= 1982,]
period2.b <- data[data$Year >= 1983 & data$Year <= 2005,]
period3.b <- data[data$Year >= 2006 & data$Year <= 2022,]

# average PHE for each period
avg_phe1.b <- period1$PHE.mean %>% mean(na.rm = TRUE)
avg_phe2.b <- period2$PHE.mean %>% mean(na.rm = TRUE)
avg_phe3.b <- period3$PHE.mean %>% mean(na.rm = TRUE)

ggplot(data = data, aes(x = Year, y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_segment(x = 1965, xend = 1982, y = avg_phe1.b, yend = avg_phe1.b, color = "red") +
  geom_segment(x = 1983, xend = 2005, y = avg_phe2.b, yend = avg_phe2.b, color = "blue") +
  geom_segment(x = 2006, xend = 2022, y = avg_phe3.b, yend = avg_phe3.b, color = "green") +
  labs(title = "Average Phenylalanine (PHE) Across Time Periods",
       x = "Year",
       y = "Average PHE") +
  theme_minimal()


#### Average GLU ###
avg_glu <- data %>% 
  group_by(Year) %>% 
  summarise(GLU = mean(GLU.mean, na.rm = TRUE))

plot(x = avg_glu$Year,
     y = avg_glu$GLU,
     xlab = "Year",
     ylab = "GLU signature",
     type = "l")

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
abline(model3, col = "red")

mod_test <- gls(PHE ~ PDO, data = merged) #if we found auto correction: correlation = corCAR1() goes into gls()
plot(ACF(mod_test, resType = "normalized"), alpha = 0.05)


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
abline(model6, col = "red")

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
abline(model7, col = "red")

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")


#### LM 8: 3 years before return 6 month lag ####
PDO_3yr_lag <- as.data.frame(matrix(nrow = length(PDO$Year), ncol = 2))
names(PDO_3yr_lag) <- c("Year","PDO")

for(i in 1:length(PDO$Year)){
  PDO_3yr_lag[i,1] <- PDO$Year[i+2]
  PDO_3yr_lag[i,2] <- mean(PDO_long[PDO_long$Year %in% c(PDO$Year[i], PDO$Year[i+1], PDO$Year[i+2],PDO$Year[i+3]) & 
                                      (PDO_long$Year == PDO$Year[i] & PDO_long$month_number >= 1 |
                                         PDO_long$Year == PDO$Year[i+1] & PDO_long$month_number <= 12 |
                                         PDO_long$Year == PDO$Year[i+2]& PDO_long$month_number <= 12|
                                         PDO_long$Year == PDO$Year[i+3]& PDO_long$month_number <= 1), "Value"])
} 
#Linear Model 8
merged <- merge(avg_phe, PDO_3yr_lag, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model8 <- lm(PHE ~ PDO, data = merged)
summary(model8)
plot(model8)
plot(x = merged$PDO,
     y = merged$PHE)
abline(model8, col = "red")

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")


#### Trophic Position ####
tp <- subset(data, select = c(Year, Trophic.Position))
avg_tp <- tp %>% 
  group_by(Year) %>% 
  summarise(Trophic.Position = mean(Trophic.Position, na.rm = TRUE))
plot(x = avg_tp$Year,
     y = avg_tp$Trophic.Position,
     type ="l")

#### LM 9: Trophic Position 2 year with lag #### 
merged <- merge(avg_tp, PDO_2yr_lag, by = "Year", all = TRUE)
names(merged) <- c("Year", "Trophic.Position", "PDO")
merged <- na.omit(merged)

model9 <- lm(Trophic.Position ~ PDO, data = merged)
summary(model9)
plot(model9)

plot(x = merged$PDO,
     y = merged$Trophic.Position)

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")
  

#### LM 10: Trophic Position 3 year with lag #### 
merged <- merge(avg_tp, PDO_3yr_lag, by = "Year", all = TRUE)
names(merged) <- c("Year", "Trophic.Position", "PDO")
merged <- na.omit(merged)

model10 <- lm(Trophic.Position ~ PDO, data = merged)
summary(model10)
plot(model10)

plot(x = merged$PDO,
     y = merged$Trophic.Position)
abline(model10, col = "red")

ggplot(data = merged, aes(x = PDO, y = PHE)) +
  geom_point() +
  geom_smooth(method = "gam")
  
  