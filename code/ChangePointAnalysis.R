setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv(file = "data.csv")

#visualize
library(ggplot2)
ggplot(data, aes(x = Year,
                 y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7)

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
