setwd("~/Documents/GitHub/CSIA_lab_work/data/final")
data <- read.csv(file = "data.csv")

library(ggplot2)
ggplot(data, aes(x = Year,
                 y = PHE.mean)) +
  geom_point(size = 3, alpha = 0.7)


