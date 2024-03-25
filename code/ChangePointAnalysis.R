setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv(file = "data.csv")

#load packages
library(changepoint)

#visualize
library(ggplot2)
ggplot(data, aes(x = Year,
                 y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7)

# Getting the data in a format that will work to create a time series object
# Convert the "Year" column to a factor to ensure correct ordering
data$Year <- factor(data$Year, levels = rev(unique(data$Year)))

# Pivot the data to wide format to create separate columns for each river and age combination
library(tidyr)
data_wide <- pivot_wider(data, names_from = System, values_from = PHE.mean)

# Create data frames for each of the river systems 
Eg <- data_wide[, c("Egegik", "Year",
                "Trophic.Position", "Age")]
Kvi <- data_wide[, c("Kvichak", "Year",
                    "Trophic.Position", "Age")]
Wood <- data_wide[, c("Wood", "Year",
                      "Trophic.Position", "Age")]


# Get rid of years from other systems
Eg <- as.data.frame(Eg[!is.na(Eg$Egegik), ])
Kvi <- as.data.frame(Kvi[!is.na(Kvi$Kvichak), ])
Wood <- as.data.frame(Wood[!is.na(Wood$Wood), ])

# Add NAs where they are missing
# This function inserts a new row at row "r" while shifting the rest of the data frame down
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

#### Running function for all missing rows for Egegik ####
newrow <- c(NA, "2019", NA, "3") 
Eg <- insertRow(Eg, newrow, r = 3)
newrow <- c(NA, "2016", NA, "3")
Eg <- insertRow(Eg, newrow, r = 5)
newrow <- c(NA, "1992", NA, "3")
Eg <- insertRow(Eg, newrow, r = 21)
newrow <- c(NA, "1986", NA, "3")
Eg <- insertRow(Eg, newrow, r = 25)
newrow <- c(NA, "1983", NA, "2")
Eg <- insertRow(Eg, newrow, r = 27)
newrow <- c(NA, "1983", NA, "3")
Eg <- insertRow(Eg, newrow, r = 27)
newrow <- c(NA, "1980", NA, "2")
Eg <- insertRow(Eg, newrow, r = 29)
newrow <- c(NA, "1980", NA, "3")
Eg <- insertRow(Eg, newrow, r = 30)
newrow <- c(NA, "1974", NA, "3")
Eg <- insertRow(Eg, newrow, r = 33)
newrow <- c(NA, "1965", NA, "3")
Eg <- insertRow(Eg, newrow, r = 39)

#### Running function for all missing rows for Kvichak ####
newrow <- c(NA, "2016", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 5)
newrow <- c(NA, "2010", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 9)
newrow <- c(NA, "2007", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 11)
newrow <- c(NA, "1998", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 17)
newrow <- c(NA, "1995", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 19)
newrow <- c(NA, "1993", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 21)
newrow <- c(NA, "1989", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 23)
newrow <- c(NA, "1986", NA, "2") 
Kvi <- insertRow(Kvi, newrow, r = 25)
newrow <- c(NA, "1986", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 25)
newrow <- c(NA, "1983", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 27)
newrow <- c(NA, "1980", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 29)
newrow <- c(NA, "1977", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 31)
newrow <- c(NA, "1974", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 33)
newrow <- c(NA, "1968", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 37)
newrow <- c(NA, "1965", NA, "3") 
Kvi <- insertRow(Kvi, newrow, r = 39)

#### Running function for all missing rows for Wood ####
newrow <- c(NA, "2019", NA, "3") 
Wood <- insertRow(Wood, newrow, r = 3)
newrow <- c(NA, "2010", NA, "3")
Wood <- insertRow(Wood, newrow, r = 9)
newrow <- c(NA, "2007", NA, "3")
Wood <- insertRow(Wood, newrow, r = 11)
newrow <- c(NA, "1998", NA, "3")
Wood <- insertRow(Wood, newrow, r = 17)
newrow <- c(NA, "1984", NA, "3")
Wood <- insertRow(Wood, newrow, r = 27)
newrow <- c(NA, "1980", NA, "3")
Wood <- insertRow(Wood, newrow, r = 29)
newrow <- c(NA, "1974", NA, "3")
Wood <- insertRow(Wood, newrow, r = 33)
newrow <- c(NA, "1965", NA, "3")
Wood <- insertRow(Wood, newrow, r = 39)


#### Create Time Series Objects ####
ts_Wood <- ts(as.numeric(Wood[Wood$Age == "2", "Wood"]),
              start = Wood$Year[40],
              end = Wood$Year[1],
              frequency = 1)

cpt_result <- cpt.mean(as.numeric(ts_Wood))

# Reverse the order of rows in the data frame so that the oldest year is at the top
Eg <- Eg[order(Eg$Year, decreasing = TRUE), ]
ts_Eg <- ts(Eg$Egegik, 
            start = Eg$Year[40], 
            end = Eg$Year[1],
            frequency = 1)

cpt_result <- cpt.mean(as.numeric(ts_Eg))

plot(cpt_result, cpt.col = "blue")


#### Analysis Using Full Data Set ####

# Average PHE across all ages and systems (should have one data point per year)
data[c(113,114), 2] = "1968" #changing the two 1967s to 1968s to have consistent spacing of years 
data[c(79,80), 2] = "1983"
data[c(61,62), 2] = "1992"

library(dplyr)
avg_data_all <- data %>%
  group_by(Year) %>%
  summarise(avg_PHE = mean(PHE.mean, na.rm = TRUE))

print(avg_data_all)
plot(avg_data_all, pch = 1)

# Convert data to a time series object
ts_data <- ts(avg_data_all$avg_PHE, 
              start = avg_data_all$Year[1],
              end = avg_data_all$Year[length(avg_data_all$Year)], 
              frequency = 1)
plot(ts_data)

# Scale the data so the variance is 1 and mean is 0
ts_1_scale <- cpt.mean(as.vector(scale(ts_data)))
cpts(ts_1_scale)


# Perform change point analysis using the 'cpt.mean' function
cpt_result <- cpt.mean(ts_data)

# Plot the change point analysis results
plot(cpt_result, cpt.col = "blue")
summary(cpt_result)

# Average PHE across all systems with only age 2
age.2 <- data[data$Age == "2",]

library(dplyr)
avg_data_2 <- age.2 %>%
  group_by(Year) %>%
  summarise(PHE.mean = mean(PHE.mean, na.rm = TRUE))

# Convert data to a time series object
ts_data_2 <- ts(avg_data_2$PHE.mean, 
              start = avg_data_2$Year[1],
              end = avg_data_2$Year[length(avg_data_2$Year)], 
              frequency = 1)
plot(ts_data_2)

# Scale the data so the variance is 1 and mean is 0
set.seed(1)
m1 <- c(rnorm(100,0,1), rnorm(100,5,1))
m1.amoc <- cpt.mean(m1)
cpts(m1.amoc)


ts_2_scale <- cpt.mean(as.vector(scale(ts_data_2)))
cpts(ts_2_scale)

# Perform change point analysis using the 'cpt.mean' function
cpt_result_2 <- cpt.mean(ts_data_2)

# Plot the change point analysis results
plot(cpt_result_2, cpt.col = "blue")
summary(cpt_result)


# Average PHE for Wood with both age classes
library(dplyr)
Wood.all <- Wood %>%
  group_by(Year) %>%
  summarise(PHE = mean(Wood, na.rm = TRUE))

# Convert data to a time series object
ts_data_2 <- ts(avg_data_2$PHE.mean, 
                start = avg_data_2$Year[1],
                end = avg_data_2$Year[length(avg_data_2$Year)], 
                frequency = 1)
plot(ts_data_2)

# Perform change point analysis using the 'cpt.mean' function
cpt_result_2 <- cpt.mean(ts_data_2)

# Plot the change point analysis results
plot(cpt_result_2, cpt.col = "blue")
summary(cpt_result)

