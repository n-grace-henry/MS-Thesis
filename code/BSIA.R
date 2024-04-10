#processing the bulk data using the HEEL package

devtools::install_github("gholtgrieve/HEEL")
rm(list=ls())
library(HEEL)
HEEL::EA.NACHO("~/Documents/GitHub/CSIA_lab_work/data/EA results/cleaned/240403_GHenry_20240403.csv")
none

HEEL::EA.NACHO("221116_GHenry_EA.csv")
none

a <- read.csv(file = "~/Documents/Grad School /SIA/HEEL_NACHO_program/Data/220725_LElmstrom_EA.csv")
HEEL::EA.NACHO("~/Documents/Grad School /SIA/HEEL_NACHO_program/Data/230809_GHenry_EA.csv")
