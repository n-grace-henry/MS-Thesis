# Load libraries
library(tidyverse)
library(patchwork)

# Load raw data

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
SaA <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA/Mean_SaA_esc_ocean2s_by_year_BB_wide.csv")

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

# PHE data format for panel plot
PHE_BB <- subset(PHE.long, System == unique(PHE.long$System)[4])
PHE_W <- subset(PHE.long, System == unique(PHE.long$System)[1])
PHE_K <- subset(PHE.long, System == unique(PHE.long$System)[2])
PHE_E <- subset(PHE.long, System == unique(PHE.long$System)[3])

# PHE plots
PHEBB <- ggplot(PHE_BB, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Bristol Bay",
       y = "Phenylalanine") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(a)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

PHEW <- ggplot(PHE_W, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Wood") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(b)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

PHEK <- ggplot(PHE_K, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Kvichak",
       xlab = "Year") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(c)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

PHEE <- ggplot(PHE_E, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Egegik") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(d)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

# Combine the plots: plot_1 on top, and plot_2, plot_3, plot_4 in one row below
combined_plot <- PHEBB / (PHEW | PHEK | PHEE) + plot_layout(heights = c(2, 1))

# Display the combined plot
combined_plot

# GLU data format for panel plot
GLU_BB <- subset(GLU.long, System == unique(GLU.long$System)[4])
GLU_W <- subset(GLU.long, System == unique(GLU.long$System)[1])
GLU_K <- subset(GLU.long, System == unique(GLU.long$System)[2])
GLU_E <- subset(GLU.long, System == unique(GLU.long$System)[3])

# GLU plots 
GLUBB <- ggplot(GLU_BB, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(title = "Bristol Bay",
       y = "Glutamic Acid") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman")
  ) +
  annotate("text", x = Inf, y = Inf, label = "(a)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

GLUW <- ggplot(GLU_W, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(title = "Wood") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(b)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

GLUK <- ggplot(GLU_K, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(title = "Kvichak") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(c)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

GLUE <- ggplot(GLU_E, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(title = "Egegik") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(d)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

# Combine the plots: plot_1 on top, and plot_2, plot_3, plot_4 in one row below
combined_plot <- GLUBB / (GLUW | GLUK | GLUE) + plot_layout(heights = c(2, 1))

# Display the combined plot
combined_plot


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

# Format data
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
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
        ) +
  annotate("text", x = Inf, y = Inf, label = "(a)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman") 

plot_W <- ggplot(anomaly_W, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +
  labs(title = "Wood") +
  theme_classic() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(b)", hjust = 1.1, vjust = 1, size = 4, family = "Times New Roman")

plot_K <- ggplot(anomaly_K, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +
  labs(title = "Kvichak") +
  theme_classic() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),  # Centers title and sets font
    text = element_text(family = "Times New Roman")
    ) +
  annotate("text", x = Inf, y = Inf, label = "(c)", hjust = 1.1, vjust = 1, size = 4, family = "Times New Roman")

plot_E <- ggplot(anomaly_E, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "lightgrey", "FALSE" = "darkgrey")) +
  labs(title = "Egegik") +
  theme_classic() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),  # Centers title and sets font
    text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = Inf, y = Inf, label = "(d)", hjust = 1.1, vjust = 1, size = 4, family = "Times New Roman")

# Combine the plots: plot_1 on top, and plot_2, plot_3, plot_4 in one row below
combined_plot <- plot_BB / (plot_W | plot_K | plot_E) + plot_layout(heights = c(2, 1))

# Display the combined plot
combined_plot
