#processing the bulk data using the HEEL package

devtools::install_github("gholtgrieve/HEEL")
rm(list=ls())
library(HEEL)
HEEL::EA.NACHO("~/Documents/GitHub/CSIA_lab_work/data/EA results/cleaned/240329_GHenry_20240329.csv")
both






