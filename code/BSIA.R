#processing the bulk data using the HEEL package

devtools::install_github("gholtgrieve/HEEL")
rm(list=ls())
library(HEEL)
HEEL::EA.NACHO("230809_GHenry_EA.csv")
none

HEEL::EA.NACHO("221116_GHenry_EA.csv")
none