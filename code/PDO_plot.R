# Load libraries 
library(tidyverse) 

# Set working directory 
setwd("~/Documents/GitHub/CSIA_lab_work/data/environmental")

# Load data
data <- read.csv("PDO_tidy.csv")

# Plot data
ggplot(data, aes(x = Year, y = Value, fill = Value > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1977, linetype = "dashed", color = "black") +
  geom_col() +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_fill_manual(values = c("TRUE" = "red2", "FALSE" = "darkblue")) +
  labs(title = "Pacific Decadal Oscillation (PDO) Index",
       x = "Year",
       y = "PDO Index") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) + 
  annotate("text", x = 1980.5, y = -1.79, label = bquote(bold("1977 Shift")), family = "Times New Roman")

