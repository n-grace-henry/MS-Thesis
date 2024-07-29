# Load packages 
library(tidyverse)
library(MARSS)
library(ggplot2)

# Load state data 
states <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states2.csv")

# Load environmental data 
PDO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")
ENSO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/ENSO_tidy.csv")
NPGO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/NPGO_tidy.csv")
ret <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/BB_returns.csv")
ice <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/environmental/sea_ice.csv")

# Put all envi data into one df 
climate <- merge(PDO, NPGO, by = "Year")
climate <- climate[,-c(2,4)]
colnames(climate) <- c("Year", "PDO", "NPGO")

# Separate data into three dfs
phe <- states[,c("years", "W.phe", "E.phe", "K.phe")]
glu <- states[,c("years", "W.glu", "E.glu", "K.glu")]
tp <- states[,c("years", "W.tp", "E.tp", "K.tp")]

# Convert to long format for plotting 
phe_long <- phe %>% pivot_longer(cols = c("W.phe", "E.phe", "K.phe"), 
                                       names_to = "System",
                                       values_to = "PHE")
glu_long <- glu %>% pivot_longer(cols = c("W.glu", "E.glu", "K.glu"),
                                       names_to = "System",
                                       values_to = "GLU")
tp_long <- tp %>% pivot_longer(cols = c("W.tp", "E.tp", "K.tp"),
                                       names_to = "System",
                                       values_to = "TP")

# Plot baseline data for all systems 
ggplot(data = phe_long, aes(x = years, y = PHE, color = System)) +
  geom_line() +
  labs(title = "PHE data for all systems",
       x = "Year",
       y = "PHE") +
  theme_minimal()

# Plot GLU data for all systems 
ggplot(data = glu_long, aes(x = years, y = GLU, color = System)) +
  geom_line() +
  labs(title = "GLU data for all systems",
       x = "Year",
       y = "GLU") +
  theme_minimal()

# Plot trophic position for all systems 
ggplot(data = tp_long, aes(x = years, y = TP, color = System)) +
  geom_line() +
  labs(title = "Trophic position data for all systems",
       x = "Year",
       y = "TP") +
  theme_minimal()

# Model environmental covariates vs states 

# Format response variable (phe)
dat <- t(phe[-1, c("W.phe", "E.phe", "K.phe")])
covariates <- t(fulldat[years, c("Temp", "TP")])

# Z-score the response variables (phe) 
the.mean <- apply(dat, 1, mean, na.rm = TRUE)
the.sigma <- sqrt(apply(dat, 1, var, na.rm = TRUE))
dat <- (dat - the.mean) * (1/the.sigma)






