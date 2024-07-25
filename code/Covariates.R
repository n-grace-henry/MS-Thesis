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
ggplot(data = states, aes())





# Plot GLU data for all systems 


# Plot trophic position for all systems 


df <- data.frame(
  Year = 2000:2005,
  W.iso = runif(6, -30, 0),
  E.iso = runif(6, -30, 0),
  K.iso = runif(6, -30, 0),
  W.tp = runif(6, 2, 5),
  E.tp = runif(6, 2, 5),
  K.tp = runif(6, 2, 5)
)

df_long <- df %>%
  pivot_longer(cols = -Year,
               names_to = c("System", ".value"),
               names_pattern = "(.+)\\.(.+)")

