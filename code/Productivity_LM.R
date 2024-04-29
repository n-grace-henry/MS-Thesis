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
phe_ret <- na.omit(phe_ret)

phe_ret_mod <- lm(TOTAL ~ PHE, data = phe_ret)
summary(phe_ret_mod)
plot(phe_ret_mod)

plot(x = phe_ret$PHE,
     y = phe_ret$TOTAL)
abline(phe_ret_mod, col = "red")



