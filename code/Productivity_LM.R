setwd("~/Documents/GitHub/CSIA_lab_work/data")

#Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Load data
returns <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/Environmental/BB_returns.csv")
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/data.csv")

#### Average PHE ###
avg_phe <- data %>% 
  group_by(Year) %>% 
  summarise(PHE = mean(PHE.mean, na.rm = TRUE))

#### PHE vs Returns ####
phe_ret <- merge(avg_phe, returns, by = "Year", all = TRUE)



merged <- merge(avg_phe, PDO_1yr_ret, by = "Year", all = TRUE)
names(merged) <- c("Year", "PHE", "PDO")
merged <- na.omit(merged)

model3 <- lm(PHE ~ PDO, data = merged)
summary(model3)
plot(x = merged$PDO,
     y = merged$PHE)
abline(model3, col = "red")



