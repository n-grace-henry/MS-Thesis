# Load libraries 
library(tidyverse) 

# Set working directory 
setwd("~/Documents/GitHub/CSIA_lab_work/data/environmental")

# Load data
data <- read.csv("PDO_tidy.csv")

# Plot data
ggplot(data, aes(x = Year, y = Value)) +
  geom_line() +
  labs(title = "Pacific Decadal Oscillation (PDO) Index",
       x = "Year",
       y = "PDO Index") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
