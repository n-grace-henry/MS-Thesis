# repository overview
Repository for all data, code and process for the lab work and analysis of AK salmon scales for Grace Henry's Masters thesis work. 

## code folder
The code folder includes R scripts to clean data and apply a drift correction. The data must match a certain format prior to running the code. The raw file must have seven columns as follows: Analysis, Identifier 1, Rt, Area All, 29N2/28N2, 15N/14N, and Peak_ID. The exact name of the column does not matter, but the order of columns does. All reference peaks and unidentified peaks should be removed. This code is only sufficient for analyzing 5 amino acids: alanine, valine, norleucine, glutamic acid and phenylalanine. 

Scripts should be run in a certain order to produce the finalized data. First the script DriftCorrection_outliers.R should be run. This script takes cleaned data (cleaned as above), first corrects d15N to the international standard reference value of air, then corrects for any drifting that may have occurred over the course of the individual run, and lastly removes outliers in this particular run. This outputs data into a folder called outliers_removed. 

The next step is to move to the R script called ConsolidateTriplicates.R. This script take all the injections of single samples, either two or three injections, and averages to get a single data point. Before running this script, all data should be in the folder outliers_removed, because this script will compile all this data. 



### final folder
This folder holds my final .csv finals that I use in analysis. 

main.clean.csv -> this is all my drift corrected and triplicate combined data. This file still has duplicates and replicates. 

main.data.csv -> this is the step after main.clean.csv where I have taken that file and combined all the replicate and duplicate samples and saved into this file. 

replicates.csv -> this file is all the replicate data before I average it with the cooresponding sample and delete the replicates from the data file. 

## progress_logs
