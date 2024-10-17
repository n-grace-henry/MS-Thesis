# Make a data frame that has the hidden state data from MARSS
# make a facet wrap plot with four panels, one for each system and one for BB as a whole

# Load libraries
library(tidyverse)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")

# Separate into three data frames
tp <- data %>% select(Year, tp.W, tp.K, tp.E, BB.tp)
colnames(tp) <- c("Year", "Wood", "Kvichak", "Egegik", "Bristol Bay")
PHE <- data %>% select(Year, W.PHE, K.PHE, E.PHE, BB.PHE)
colnames(PHE) <- c("Year", "Wood", "Kvichak", "Egegik", "Bristol Bay")
GLU <- data %>% select(Year, W.GLU, K.GLU, E.GLU, BB.GLU)
colnames(GLU) <- c("Year", "Wood", "Kvichak", "Egegik", "Bristol Bay")

# Convert all to long format
tp.long <- tp %>% pivot_longer(cols = -Year, names_to = "System", values_to = "tp")
PHE.long <- PHE %>% pivot_longer(cols = -Year, names_to = "System", values_to = "PHE")
GLU.long <- GLU %>% pivot_longer(cols = -Year, names_to = "System", values_to = "GLU")

# TP plot 
ggplot(tp.long, aes(x = Year, y = tp, color = System)) +
  geom_line() +
  facet_wrap(~System) +
  labs(title = "Trophic Position Over Time",
       x = "Year",
       y = "Trophic Position") +
  theme_minimal() + 
  theme(legend.position = "none")

# PHE plot
ggplot(PHE.long, aes(x = Year, y = PHE, color = System)) +
  geom_line() +
  facet_wrap(~System) +
  labs(title = "Phenylalanine Through Time",
       x = "Year",
       y = "Phenylalanine d15N/d14N") +
  theme_minimal() + 
  theme(legend.position = "none")

# GLU plot
ggplot(GLU.long, aes(x = Year, y = GLU, color = System)) +
  geom_line() +
  facet_wrap(~System) +
  labs(title = "Glutamic Acid Through Time",
       x = "Year",
       y = "Glutamic Acid d15N/d14N") +
  theme_minimal() + 
  theme(legend.position = "none")

# Anomaly plot of tp 
anomaly <- function(data){
  df <- matrix(nrow = length(data), ncol = 2)
  
  for(i in 1:length(data)){
    df[i, 1] <- mean(data)
    df[i, 2] <- data[i] - df[i, 1]
  }
  
  return(df)
}

# Use function
W.anomaly <- anomaly(data$tp.W)
K.anomaly <- anomaly(data$tp.K)
E.anomaly <- anomaly(data$tp.E)
BB.anomaly <- anomaly(data$BB.tp)

# Make data frame for plotting
anomaly <- data.frame(Year = data$Year, Wood = W.anomaly[,2], Kvichak = K.anomaly[,2], Egegik = E.anomaly[,2], BristolBay = BB.anomaly[,2])

# Pivot to long format
anomaly.long <- anomaly %>% pivot_longer(cols = -Year, names_to = "System", values_to = "Anomaly")

# Plot
ggplot(anomaly.long, aes(x = Year, y = Anomaly)) +
  geom_ribbon(aes(ymin = pmin(Anomaly, 0), ymax = 0), fill = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = pmax(Anomaly, 0)), fill = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_line() +
  facet_wrap(~System) +
  labs(title = "Trophic Position Anomaly Over Time",
       x = "Year",
       y = "Trophic position anomaly") +
  theme_minimal() 




