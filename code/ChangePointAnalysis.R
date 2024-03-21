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

# Separate by Age class
Eg_2 <- Eg[Eg$Age == "2",]
Eg_3 <- Eg[Eg$Age == "3",]


#### Analysis Using Full Data Set ####

# Make a column of average PHE across all ages and systems (should have one data point per year)

# Remove rows with NAs
data <- na.omit(data)

# Reverse the order of rows in the data frame so that the oldest year is at the top
data <- data[order(data$Year, decreasing = TRUE), ]

# Convert data to a time series object
ts_data <- ts(data$PHE.mean, 
              start = data$Year[length(data$Year)], 
              end = data$Year[1], 
              frequency = 6)

# Perform change point analysis using the 'cpt.mean' function
cpt_result <- cpt.mean(ts_data)

# Plot the change point analysis results
plot(cpt_result, cpt.col = "blue")

summary(cpt_result)
