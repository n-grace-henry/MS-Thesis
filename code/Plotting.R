# Make a data frame that has the hidden state data from MARSS
# make a facet wrap plot with four panels, one for each system and one for BB as a whole

# Load libraries
library(tidyverse)
library(patchwork)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
SaA <- SaA_BB <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA/Mean_SaA_esc_ocean2s_by_year_BB_wide.csv")

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


library(ggplot2)
library(patchwork)

# Assuming you have four systems in your 'anomaly.long' dataset
anomaly_BB <- subset(anomaly.long, System == unique(anomaly.long$System)[4])
anomaly_W <- subset(anomaly.long, System == unique(anomaly.long$System)[1])
anomaly_K <- subset(anomaly.long, System == unique(anomaly.long$System)[2])
anomaly_E <- subset(anomaly.long, System == unique(anomaly.long$System)[3])

# Create individual plots
plot_BB <- ggplot(anomaly_BB, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +  
  labs(title = "Bristol Bay",
       x = "Year",
       y = "TP anomaly") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
        ) 

plot_W <- ggplot(anomaly_W, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +
  labs(title = "Wood") +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),  # Centers title and sets font
    text = element_text(family = "Times New Roman") 
  ) 

plot_K <- ggplot(anomaly_K, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +
  labs(title = "Kvichak") +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),  # Centers title and sets font
    text = element_text(family = "Times New Roman") 
  )

plot_E <- ggplot(anomaly_E, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +
  labs(title = "Egegik") +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),  # Centers title and sets font
    text = element_text(family = "Times New Roman") 
  )

# Combine the plots: plot_1 on top, and plot_2, plot_3, plot_4 in one row below
combined_plot <- plot_BB / (plot_W | plot_K | plot_E) + plot_layout(heights = c(2, 1))

# Display the combined plot
combined_plot
