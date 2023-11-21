rm(list = ls())
setwd("~/Documents/Grad School /CSIA/code/processed")

oct12 <- read.csv(file = "20231012_corrected.csv")
aug29 <- read.csv(file= "20230829_corrected.csv")
sep24 <- read.csv(file= "20230924_corrected.csv")
sep22 <- read.csv(file="20230922_corrected.csv")

"04-E-2-R" <- oct12[1,"PHE.mean"]
"04-E-2" <- sep22[1,"PHE.mean"]

"10-W-2-R" <- oct12[2,"PHE.mean"]
"10-W-2" <- sep24[3, "PHE.mean"]

"22-K-3-R" <- oct12[3, "PHE.mean"]
"22-K-3" <- aug29[1, "PHE.mean"]

oct12[1,"GLU.mean"]
sep22[1,"GLU.mean"]

oct12[2,"GLU.mean"]
sep24[3, "GLU.mean"]

oct12[3, "GLU.mean"]
aug29[1, "GLU.mean"]

setwd("~/Documents/Grad School /CSIA/code")

library(dplyr)
library(readr)

data <- read.csv(file="main.data")

