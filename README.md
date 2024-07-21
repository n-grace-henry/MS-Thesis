# Repository Overview
Repository for all CSIA-AA data, code and analysis of AK salmon scales for Grace Henry's Masters thesis at the University of Washington in the Holtgrieve and Schindler labs. 

## code folder
All code used to clean, correct and analyze raw data. Raw data files must be fomatted with seven columns as follows: Analysis, Identifier 1, Rt, Area All, 29N2/28N2, 15N/14N, and Peak_ID and be in csv format. All reference peaks and unidentified peaks should be removed prior to reading into any of these scripts. The analysis run in this repo accounts for only  5 amino acids: alanine, valine, norleucine, glutamic acid and phenylalanine. 

Below are descriptions of the included scripts along with their products: 

DriftCorrection.R -> Takes raw data files (formatted as outlined above), corrects to international N air standard and drift corrects. Saves each drift corrected run into a folder called "processed". 

DriftCorrection_ouliers -> Takes raw data files (formatted as outlined above) and applies a drift correction with NO CONSOLIDATION of triplicates. Requires manual inspection of each triplicate set to determine what is characterized as an outlier. Saves data, unconsolidated, in the "outliers_removed" folder, naming each file by it's run date. 

Mass_Effects.R -> Corrects for area effects on d15N signature. Compiles "processed" files into one csv called "data_full.csv". Identifies outlier points in the PHE data that get removed from the final produced file. Corrects for the apparent low area - low d15N pattern of GLU by fitting a second order polynomial to the data. Saves data frame in the final folder as "mass_correct.csv".

ConsolidateTriplicates.R -> Takes "mass_correct.csv" and consolidates triplicate samples. Adds Year, System and Age columns. Writes new csv called "consolidated.csv".  

Duplicates.R -> Takes a full data file that has been drift corrected and consolidated already. Creates a function to average the values of all replicate samples and duplicate and replace with only one value in the data sheet. Replicates are defined as the second half of scales that were run because of issues with the first set or simply for the sake of rerunning. Duplicates are defined as samples run more than once from the same sample bottle. Trophic position calculations happen in this script. This is the final step of scripts that should be run to finalize the data file. Writes csv called "all_correct_final.csv". 

### data folder
Contains data folders at different levels of processing. 

raw -> Excel files straight from the GC. Amino acid peaks have been IDed. 

cleaned -> Csv files of raw data that has been formatted for drift correction script.  

processed -> Csv files that have been drift corrected but has not been consolidated. This also still has the area and other info beyond just delta values. 

outliers -> Csv files for each run that had outlier triplicate values removed by hand, meaning that each injection was looked at, and if anything stood out as an outlier it was removed. 

## work - flow 
correct to Nair, in same script then drift correct with no consolidation. Mass correct drift corrected data with no consolidation.  

### final folder
replicates.csv -> this file is all the replicate data before I average it with the cooresponding sample and delete the replicates from the data file. 

data_full.csv -> raw data has been drift corrected but not consolidated. All data in this sheet including all AAs and duplicates and replicates. 

mass_correct.csv -> data that has been drift correct, corrected to Nair, and mass corrected, but has not yet been consolidated.

all_correct_final.csv -> data that has undergone all corrections including mass correction, drift correction and correction to Nair, consolidation and removal of dupes and reps. 

trophic_position.csv -> file with trophic position calculated from three different methods; from raw data, from time period specified baseline, from modeled baseline. Calculations done in the R script "Trophic.Position.R".

full.csv <- all corrections have been done, but triplicates have not been consolidated and replicates have not been removed. Formatted so that this has a Year, System, and Age column. 

consolidated.csv -> consolidated data with GLU and PHE adjusted data only. Formatted with Year, Age and System columns. Duplicates and triplicates have not been removed. 

states2.csv -> csv file with final states of PHE baselines and TP from age 2 data generated via univariate state-space models. 

