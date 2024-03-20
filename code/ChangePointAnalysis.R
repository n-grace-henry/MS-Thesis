setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv(file = "data.csv")

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

# Running function for all missing rows
newrow <- c(NA, "2019", NA, "3") #insert for Egegik 
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

newrow <- c(NA, "2016", NA, "3") #repeat for Kvichak
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

# Separate by Age class
Eg_2 <- Eg[Eg$Age == "2",]
Eg_3 <- Eg[Eg$Age == "3",]

# Create separate time series objects for each river and age combination
ts_river1_age2 <- ts(data_wide$Egegik[data_wide$Age == 2], 
                     start = 2022, frequency = 1)
ts_river1_age3 <- ts(data_wide$River1[data_wide$age == 3], 
                     start = 2022, frequency = 1)
ts_river2_age2 <- ts(data_wide$River2[data_wide$age == 2], 
                     start = 2022, frequency = 1)
ts_river2_age3 <- ts(data_wide$River2[data_wide$age == 3], 
                     start = 2022, frequency = 1)
ts_river3_age2 <- ts(data_wide$River3[data_wide$age == 2], 
                     start = 2022, frequency = 1)
ts_river3_age3 <- ts(data_wide$River3[data_wide$age == 3], 
                     start = 2022, frequency = 1)








#load packages
library(changepoint)

# Remove rows with NAs
data <- na.omit(data)

# Reverse the order of rows in the data frame so that the oldest year is at the top
data <- data[order(data$Year, decreasing = TRUE), ]

# Convert data to a time series object
ts_data <- ts(data$PHE.mean, start = data$Year[length(data$Year)], end = data$Year[1])

# Perform change point analysis using the 'cpt.mean' function
cpt_result <- cpt.mean(ts_data)

# Plot the change point analysis results
plot(cpt_result, cpt.col = "blue")

summary(cpt_result)
