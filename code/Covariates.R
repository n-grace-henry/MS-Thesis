# Load packages 
library(tidyverse)

# Load state data 
states <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/states2.csv")

# Load environmental data 


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

# Model envi covariates and states 




