# Repository Overview
Repository for all CSIA-AA data, code and analysis of AK salmon scales for Grace Henry's Masters thesis at the University of Washington in the Holtgrieve and Schindler labs. 

## code folder
All code used to clean, correct and analyze raw data. Raw data files must have seven columns as follows: Analysis, Identifier 1, Rt, Area All, 29N2/28N2, 15N/14N, and Peak_ID and be in csv format. All reference peaks and unidentified peaks should be removed prior to reading into any of these scripts. The analysis run in this repo accounts for only with 5 amino acids: 5 amino acids: alanine, valine, norleucine, glutamic acid and phenylalanine. 

Below are descriptions of the included scripts along with their products. 

DriftCorrection.R -> Takes raw data files (formatted as outlined above) and both drift corrects and consolidates triplicates, after which it puts the corrected data in the "with_outliers" folder, naming each file by it's run date. 

DriftCorrection_ouliers -> Takes raw data files (formatted as outlined above) and applies a drift correction with NO CONSOLIDATION of triplicates. Requires manual inspection of each triplicate set to determine what is characterized as an outlier. Saves data, unconsolidated, in the "outliers_removed" folder, naming each file by it's run date. 

ConsolidateTriplicates -> Compiles all individual drift corrected files in a folder (outliers_removed or other) and consolidates triplicates. Adds a year, system and age class column to the data frame and writes new csv. 

Duplicates.R -> Takes a full data file that has been drift corrected and consolidated already. Creates a function to average the values of all replicate samples and duplicate and replace with only one value in the data sheet. Replicates are defined as the second half of scales that were run because of issues with the first set or simply for the sake of rerunning. Duplicates are defined as samples run more than once from the same sample bottle. 

Mass_Effects.R -> Corrects for area effects on d15N signature. First drift corrects the raw data but does not consolidate, and saves each run in a folder called "processed". Compiles these files into one csv called "data_full.csv" and saves this in the final folder. Identifies outlier points in the PHE data that get removed from the final produced file. Corrects for the apparent low area - low d15N pattern of GLU by fitting a second order polynomial to the data. Saves data frame in the final folder as "mass_correct.csv".

### data folder
Contains data folders at different levels of processing. 

cleaned -> raw data that has been cleaned in the original excel file and converted to a csv file. This is in the correct format for imputting into the drift correction scripts. 

raw -> straight from the GC. This has been formatted prior to being changed to a csv. Peaks have been IDed. 

processed -> this is data that has been drift corrected but has not been consolidated. This also still has the area and other info beyond just delta values. 


### final folder
This folder holds my final .csv finals that I use in analysis. 

main.clean.csv -> this is all my drift corrected and triplicate combined data. This file still has duplicates and replicates. 

main.data.csv -> this is the step after main.clean.csv where I have taken that file and combined all the replicate and duplicate samples and saved into this file. 

replicates.csv -> this file is all the replicate data before I average it with the cooresponding sample and delete the replicates from the data file. 

all.data.csv -> all corrected data including the points that did not end up getting data. Full time series data including the NA points. 

data.csv -> same as the all.data.csv file except I removed the run on 2/7/24 that I think was wrong. This run looked pretty bad and I believe it was because the source blew and there might have also been a leak in the system. It gave some crazy high values and I think it is safer to remove this run entirely. 

data_full.csv -> raw data has been drift corrected but not consolidated. All data in this sheet including AAs and duplicates and replicates. 

mass_correct_full.csv -> mass corrected data (only has GLU and PHE signatures). Has been consolidated from triplicate but duplicates and replicates have not been removed. 

all_correct_final.csv -> data that has undergone all corrections including mass correction, drift correction and correction to Nair. 

