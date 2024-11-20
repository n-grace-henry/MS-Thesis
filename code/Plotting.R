# Load libraries
library(tidyverse)
library(patchwork)
library(zoo)

# Load raw data

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
SaA <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/SaA/Mean_SaA_esc_ocean2s_by_year_BB_wide.csv")
pink <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/Pink.csv")

# Trim pink data for appropriate years
pink <- subset(pink, Year >= 1965 & Year <= 2022)

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
PHE_BB$size <- SaA$mean_SaA
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
    df[i, 1] <- mean(data, na.rm = TRUE)
    df[i, 2] <- data[i] - df[i, 1]
  }
  return(df)
}

# Use function
W.anomaly <- anomaly(data$tp.W)
K.anomaly <- anomaly(data$tp.K)
E.anomaly <- anomaly(data$tp.E)
BB.anomaly <- anomaly(data$BB.tp)
size.anomaly <- anomaly(SaA$mean_SaA)

# Make data frame for plotting
anomaly.df <- data.frame(Year = data$Year, Wood = W.anomaly[,2], Kvichak = K.anomaly[,2], Egegik = E.anomaly[,2], BristolBay = BB.anomaly[,2])

# Pivot to long format
anomaly.long <- anomaly.df %>% pivot_longer(cols = -Year, names_to = "System", values_to = "Anomaly")

# Format data
anomaly_BB <- subset(anomaly.long, System == unique(anomaly.long$System)[4])
anomaly_BB$size <- size.anomaly[,2]
anomaly_BB$avg_size <- rollmean(anomaly_BB$size, k = 5, align = "center", na.pad = TRUE)
a.pink <- anomaly(pink$Pink)
anomaly_BB$pink <- rollmean(a.pink[,2], k = 5, align = "center", na.pad = TRUE)

anomaly_W <- subset(anomaly.long, System == unique(anomaly.long$System)[1])
anomaly_K <- subset(anomaly.long, System == unique(anomaly.long$System)[2])
anomaly_E <- subset(anomaly.long, System == unique(anomaly.long$System)[3])

# Create individual plots
plot_BB <- ggplot(anomaly_BB, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray65", "FALSE" = "gray40")) +  
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
  scale_fill_manual(values = c("TRUE" = "gray65", "FALSE" = "gray40")) +
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
  scale_fill_manual(values = c("TRUE" = "gray65", "FALSE" = "gray40")) +
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
  scale_fill_manual(values = c("TRUE" = "gray65", "FALSE" = "gray40")) +
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

# BB TP plot with SaA 
anomaly_BB$scaled_size <- anomaly_BB$avg_size * 0.0205
ggplot(anomaly_BB, aes(x = Year)) +
  geom_col(aes(y = Anomaly, fill = Anomaly > 0)) +
  geom_line(aes(y = scaled_size), color = "red2", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("TRUE" = "gray65", "FALSE" = "gray40")) +  
  scale_y_continuous(limits = c(-0.30, 0.30), breaks = c(-.3,-.15, 0, .15, .3), sec.axis = sec_axis(~ . / 0.0205, name = "Size Anomaly")) + 
  labs(title = "Bristol Bay",
       x = "Year",
       y = "Trophic Position anomaly") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman")) +
  annotate("text", x = Inf, y = Inf, label = "(a)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")

anomaly_BB$scaled_pink <- anomaly_BB$pink * 0.0013
ggplot(anomaly_BB, aes(x = Year)) +
  geom_col(aes(y = Anomaly, fill = Anomaly > 0)) +
  geom_line(aes(y = scaled_pink), color = "deepskyblue1", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("TRUE" = "gray65", "FALSE" = "gray40")) +  
  scale_y_continuous(limits = c(-0.30, 0.30), breaks = c(-.3,-.15, 0, .15, .3), sec.axis = sec_axis(~ . / 0.0013, name = "Pink Abundance Anomaly")) + 
  labs(title = "Bristol Bay",
       x = "Year",
       y = "Trophic Position anomaly") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman")) +
  annotate("text", x = Inf, y = Inf, label = "(a)", hjust = 1.1, vjust = 1, size = 5, family = "Times New Roman")




# Figure 1: Three panel plot of Bristol Bay wide data
PHE.BB <- ggplot(PHE_BB, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Bristol Bay",
       y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(PHE_BB$PHE), label = "(a) phenylalanine (source AA)", 
           hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
GLU.BB <- ggplot(GLU_BB, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman",),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(GLU_BB$GLU), label = "(b) glutamic acid (trophic AA)", 
           hjust = 0, vjust = 0.5, size = 4, family = "Times New Roman")
TP.BB <- ggplot(anomaly_BB, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +  
  labs(x = "Year",
       y = "Deviation from mean") +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(-0.28, 0.28), breaks = c(-0.30, -0.2, -0.1, 0.0, 0.1, 0.2, 0.30)) + 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 7, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 0.28, label = "(c) trophic position", 
           hjust = 0, vjust = 0.19, size = 4, family = "Times New Roman")

figure1 <- PHE.BB / GLU.BB / TP.BB 
figure1

# Figure 2: system specific
PHE.W <- ggplot(PHE_W, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Wood", 
       y = expression(delta^15*N ~ "(‰)")) +
  scale_y_continuous(limits = c(2.5, 8), breaks = c(3,4,5,6,7,8)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 8, label = "(a) phenylalanine (source AA)", hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
PHE.K <- ggplot(PHE_K, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Kvichak", 
       y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(1.6,8.9), breaks = c(2,3,4,5,6,7,8)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 8.9, label = "(b)", hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
PHE.E <- ggplot(PHE_E, aes(x = Year, y = PHE)) +
  geom_line() + 
  labs(title = "Egegik", 
       y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(1.2,7), breaks = c(2,3,4,5,6,7)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 7, label = "(c)", hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")

GLU.W <- ggplot(GLU_W, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(21.3, 26), breaks = c(22, 23, 24, 25, 26)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 26, label = "(d) glutamic acid (trophic AA)", hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
GLU.K <- ggplot(GLU_K, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) 
GLU.E <- ggplot(GLU_E, aes(x = Year, y = GLU)) +
  geom_line() + 
  labs(y = expression(delta^15*N ~ "(‰)")) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  )

plot.W <- ggplot(anomaly_W, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +
  labs(x = "Year", 
       y = "Deviation from mean") +
  scale_x_continuous(breaks = c(1965, 1985, 2005, 2022)) +
  scale_y_continuous(limits = c(-0.5, 0.36), breaks = c(-0.5, -0.25, 0, 0.25)) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 0.36, label = "(g) trophic position", hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
plot.K <- ggplot(anomaly_K, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +
  labs(x = "Year", 
       y = "Deviation from mean") +
  scale_x_continuous(breaks = c(1965, 1985, 2005, 2022)) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) 
plot.E <- ggplot(anomaly_E, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +
  labs(x = "Year", 
       y = "Deviation from mean") +
  scale_x_continuous(breaks = c(1965, 1985, 2005, 2022)) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  )

figure2 <- (PHE.W | PHE.K | PHE.E) / 
  (GLU.W | GLU.K | GLU.E) / 
  (plot.W | plot.K | plot.E)
figure2



