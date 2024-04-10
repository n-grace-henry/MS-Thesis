#processing the bulk data using the HEEL package

devtools::install_github("gholtgrieve/HEEL")
rm(list=ls())
library(HEEL)
HEEL::EA.NACHO("~/Documents/GitHub/CSIA_lab_work/data/EA results/cleaned/240403_GHenry_20240403.csv")
none

HEEL::EA.NACHO("221116_GHenry_EA.csv")
none

a <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/EA results/cleaned/240403_GHenry_20240403.csv")
HEEL::EA.NACHO("~/Documents/GitHub/CSIA_lab_work/data/EA results/cleaned/240403_GHenry_20240403.csv")


HEEL::EA.NACHO("~/Desktop/220909_SSabikunnahar_EA.csv")
both
