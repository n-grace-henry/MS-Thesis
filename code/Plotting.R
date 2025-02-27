# Load libraries
library(tidyverse)
library(patchwork)
library(zoo)

# Load data 
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states.csv")
raw <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/full.csv")

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

# GLU data format for panel plot
GLU_BB <- subset(GLU.long, System == unique(GLU.long$System)[4])
GLU_W <- subset(GLU.long, System == unique(GLU.long$System)[1])
GLU_K <- subset(GLU.long, System == unique(GLU.long$System)[2])
GLU_E <- subset(GLU.long, System == unique(GLU.long$System)[3])

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
W.anomaly.tp <- anomaly(data$tp.W)
K.anomaly.tp <- anomaly(data$tp.K)
E.anomaly.tp <- anomaly(data$tp.E)
BB.anomaly.tp <- anomaly(data$BB.tp)

W.anomaly.phe <- anomaly(data$W.PHE)
K.anomaly.phe <- anomaly(data$K.PHE)
E.anomaly.phe <- anomaly(data$E.PHE)
BB.anomaly.phe <- anomaly(data$BB.PHE)

W.anomaly.glu <- anomaly(data$W.GLU)
K.anomaly.glu <- anomaly(data$K.GLU)
E.anomaly.glu <- anomaly(data$E.GLU)
BB.anomaly.glu <- anomaly(data$BB.GLU)

# Make data frame for plotting
anomaly.df <- data.frame(Year = data$Year, Wood = W.anomaly[,2], Kvichak = K.anomaly[,2], Egegik = E.anomaly[,2], BristolBay = BB.anomaly[,2])
anomaly.df.phe <- data.frame(Year = data$Year, Wood = W.anomaly.phe[,2], Kvichak = K.anomaly.phe[,2], Egegik = E.anomaly.phe[,2], BristolBay = BB.anomaly.phe[,2])
anomaly.df.glu <- data.frame(Year = data$Year, Wood = W.anomaly.glu[,2], Kvichak = K.anomaly.glu[,2], Egegik = E.anomaly.glu[,2], BristolBay = BB.anomaly.glu[,2])

# Pivot to long format
anomaly.long <- anomaly.df %>% pivot_longer(cols = -Year, names_to = "System", values_to = "Anomaly")
anomaly.long.phe <- anomaly.df.phe %>% pivot_longer(cols = -Year, names_to = "System", values_to = "Anomaly")
anomaly.long.glu <- anomaly.df.glu %>% pivot_longer(cols = -Year, names_to = "System", values_to = "Anomaly")

# Format data
anomaly_BB <- subset(anomaly.long, System == unique(anomaly.long$System)[4])
anomaly_W <- subset(anomaly.long, System == unique(anomaly.long$System)[1])
anomaly_K <- subset(anomaly.long, System == unique(anomaly.long$System)[2])
anomaly_E <- subset(anomaly.long, System == unique(anomaly.long$System)[3])

# Format SE to be an upper and a lower bound
data$PHE_lower <- data$BB.PHE - data$PHE.SE
data$PHE_upper <- data$BB.PHE + data$PHE.SE

data$GLU_lower <- data$BB.GLU - data$GLU.SE
data$GLU_upper <- data$BB.GLU + data$GLU.SE

# Figure 1: Three panel plot of Bristol Bay wide data
PHE.BB <- ggplot(data, aes(x = Year, y = BB.PHE)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "PHE", Age == "2"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) + 
  geom_ribbon(aes(ymin = PHE_lower, ymax = PHE_upper), 
              fill = "grey", alpha = 0.3) +  
  labs(title = "Bristol Bay",
       y = expression(bold("PHE" ~ delta^15*N ~ "(‰)"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(data$BB.PHE), label = "(a)", 
           hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman") 
GLU.BB <- ggplot(data, aes(x = Year, y = BB.GLU)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "GLU", Age == "2"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) + 
  geom_ribbon(aes(ymin = GLU_lower, ymax = GLU_upper), 
              fill = "grey", alpha = 0.3) +  
  labs(y = expression(bold("GLU" ~ delta^15*N ~ "(‰)"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman",),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(GLU_BB$GLU), label = "(b)", 
           hjust = 0, vjust = 0.5, size = 4, family = "Times New Roman")
TP.BB <- ggplot(anomaly_BB, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +  
  labs(x = "Year",
       y = expression(bold("TP Anomaly"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(-0.28, 0.28), breaks = c(-0.30, -0.2, -0.1, 0.0, 0.1, 0.2, 0.30)) + 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 7, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 0.28, label = "(c)", 
           hjust = 0, vjust = 0.19, size = 4, family = "Times New Roman")

figure1 <- PHE.BB / GLU.BB / TP.BB 
figure1

# Figure 2: system specific
# Format SE to be an upper and a lower bound
data$PHE_lower_W <- data$W.PHE - data$W.PHE.SE
data$PHE_upper_W <- data$W.PHE + data$W.PHE.SE

data$PHE_lower_K <- data$K.PHE - data$K.PHE.SE
data$PHE_upper_K <- data$K.PHE + data$K.PHE.SE

data$PHE_lower_E <- data$E.PHE - data$E.PHE.SE
data$PHE_upper_E <- data$E.PHE + data$E.PHE.SE

# Repeat for GLU
data$GLU_lower_W <- data$W.GLU - data$W.GLU.SE
data$GLU_upper_W <- data$W.GLU + data$W.GLU.SE

data$GLU_lower_K <- data$K.GLU - data$K.GLU.SE
data$GLU_upper_K <- data$K.GLU + data$K.GLU.SE

data$GLU_lower_E <- data$E.GLU - data$E.GLU.SE
data$GLU_upper_E <- data$E.GLU + data$E.GLU.SE

# Plot
PHE.W <- ggplot(data, aes(x = Year, y = W.PHE)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "PHE", Age == "2", System == "Wood"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) + 
  geom_ribbon(aes(ymin = PHE_lower_W, ymax = PHE_upper_W), 
              fill = "grey", alpha = 0.3) +  
  labs(title = "Wood", 
       y = expression(bold("PHE" ~ delta^15*N ~ "(‰)"))) +
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
  annotate("text", x = 1965, y = 8.9, label = expression(bold("(a)")), hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
PHE.K <- ggplot(data, aes(x = Year, y = K.PHE)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "PHE", Age == "2", System == "Kvichak"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) +
  geom_ribbon(aes(ymin = PHE_lower_K, ymax = PHE_upper_K), 
              fill = "grey", alpha = 0.3) +  
  labs(title = "Kvichak") +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(1.6,8.9), breaks = c(2,3,4,5,6,7,8)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 8.9, label = expression(bold("(b)")), hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
PHE.E <- ggplot(data, aes(x = Year, y = E.PHE)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "PHE", Age == "2", System == "Egegik"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) +
  geom_ribbon(aes(ymin = PHE_lower_E, ymax = PHE_upper_E), 
              fill = "grey", alpha = 0.3) +  
  labs(title = "Egegik") +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(1.6,8.9), breaks = c(2,3,4,5,6,7,8)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 8.9, label = expression(bold("(c)")), hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")

GLU.W <- ggplot(data, aes(x = Year, y = W.GLU)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "GLU", Age == "2", System == "Wood"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) +
  geom_ribbon(aes(ymin = GLU_lower_W, ymax = GLU_upper_W), 
              fill = "grey", alpha = 0.3) +  
  labs(y = expression(bold("GLU" ~ delta^15*N ~ "(‰)"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(21,26), breaks = c(22,23,24,25,26)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 26, label = expression(bold("(d)")), hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
GLU.K <- ggplot(data, aes(x = Year, y = K.GLU)) +
  geom_point(data = raw %>% filter(AAID == "GLU", Age == "2", System == "Kvichak"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) +
  geom_line() + 
  geom_ribbon(aes(ymin = GLU_lower_K, ymax = GLU_upper_K), 
              fill = "grey", alpha = 0.3) +  
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(21,26), breaks = c(22,23,24,25,26)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  )  +
  annotate("text", x = 1965, y = 26, label = expression(bold("(e)")), hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")
GLU.E <- ggplot(data, aes(x = Year, y = E.GLU)) +
  geom_line() + 
  geom_point(data = raw %>% filter(AAID == "GLU", Age == "2", System == "Egegik"), 
             aes(x = Year, y = adj), 
             color = "grey", size = 2, alpha = 0.75, shape = 16) +
  geom_ribbon(aes(ymin = GLU_lower_E, ymax = GLU_upper_E), 
              fill = "grey", alpha = 0.3) +  
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(21,26), breaks = c(22,23,24,25,26)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 26, label = expression(bold("(f)")), hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman")

plot.W <- ggplot(anomaly_W, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "grey80", "FALSE" = "grey60")) +
  labs(x = "Year", 
       y = expression(bold("TP Anomaly"))) +
  scale_x_continuous(breaks = c(1965, 1985, 2005, 2022)) +
  scale_y_continuous(limits = c(-0.5, 0.55), breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 0.5, label = expression(bold("(g)")), size = 4, family = "Times New Roman")
plot.K <- ggplot(anomaly_K, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "grey80", "FALSE" = "grey60")) +
  labs(x = "Year") +
  scale_x_continuous(breaks = c(1965, 1985, 2005, 2022)) +
  scale_y_continuous(limits = c(-0.5, 0.55), breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 0.5, label = expression(bold("(h)")), size = 4, family = "Times New Roman")
plot.E <- ggplot(anomaly_E, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "grey80", "FALSE" = "grey60")) +
  labs(x = "Year") +
  scale_x_continuous(breaks = c(1965, 1985, 2005, 2022)) +
  scale_y_continuous(limits = c(-0.5, 0.55), breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  ) +
  annotate("text", x = 1965, y = 0.5, label = expression(bold("(i)")), size = 4, family = "Times New Roman")


figure2 <- (PHE.W | PHE.K | PHE.E) / 
  (GLU.W | GLU.K | GLU.E) / 
  (plot.W | plot.K | plot.E)
figure2

# Different look for figures 1 and 2
PHE.BB <- ggplot(data, aes(x = Year, y = BB.PHE)) +
  geom_line() + 
  geom_ribbon(aes(ymin = PHE_lower, ymax = PHE_upper), 
              fill = "grey", alpha = 0.3) +  
  labs(title = "Bristol Bay",
       y = expression(bold("PHE" ~ delta^15*N ~ "(‰)"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(data$BB.PHE), label = "(a)", 
           hjust = 0, vjust = 0.3, size = 4, family = "Times New Roman") 
GLU.BB <- ggplot(data, aes(x = Year, y = BB.GLU)) +
  geom_line() + 
  geom_ribbon(aes(ymin = GLU_lower, ymax = GLU_upper), 
              fill = "grey", alpha = 0.3) +  
  labs(y = expression(bold("GLU" ~ delta^15*N ~ "(‰)"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman",),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(GLU_BB$GLU), label = "(b)", 
           hjust = 0, vjust = 0.5, size = 4, family = "Times New Roman")
TP.BB <- ggplot(data, aes(x = Year, y = BB.tp)) +
  geom_line() + 
  labs(y = expression(bold("Trophic Position"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
        text = element_text(family = "Times New Roman",),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = max(data$BB.tp) + 0.02, label = "(c)", 
           hjust = 0, vjust = 0.5, size = 4, family = "Times New Roman")

newfig1 <- PHE.BB / GLU.BB / TP.BB



PHE.BB.an <- ggplot(anomaly.long.phe %>%  filter(System == "BristolBay"), aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +  
  labs(title = "Bristol Bay Anomaly",
       y = expression(bold("PHE"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 0.5, label = "(a)", 
           size = 4, family = "Times New Roman")
GLU.BB.an <- ggplot(anomaly.long.glu %>%  filter(System == "BristolBay"), aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +  
  labs(y = expression(bold("GLU"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 7, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 1, label = "(b)", 
            size = 4, family = "Times New Roman")
TP.BB.an <- ggplot(anomaly_BB, aes(x = Year, y = Anomaly, fill = Anomaly > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray60")) +  
  labs(x = "Year",
       y = expression(bold("Trophic Position"))) +
  scale_x_continuous(breaks = c(1965, 1975, 1985, 1995, 2005, 2015, 2022)) +
  scale_y_continuous(limits = c(-0.28, 0.28), breaks = c(-0.30, -0.2, -0.1, 0.0, 0.1, 0.2, 0.30)) + 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 7, family = "Times New Roman"), 
        text = element_text(family = "Times New Roman"),
        axis.line.y = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
  ) +
  annotate("text", x = 1965, y = 0.28, label = "(c)", 
           hjust = 0, vjust = 0.19, size = 4, family = "Times New Roman")

fig <- PHE.BB.an / GLU.BB.an / TP.BB.an

