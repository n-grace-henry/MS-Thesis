# repository overview
Repository for all data, code and process for the lab work and analysis of AK salmon scales for Grace Henry's Masters thesis work. 

## code folder
The code folder includes R scripts to clean data and apply a drift correction. The data must match a certain format prior to running the code. The raw file must have seven columns as follows: Analysis, Identifier 1, Rt, Area All, 29N2/28N2, 15N/14N, and Peak_ID. The exact name of the column does not matter, but the order of columns does. All reference peaks and unidentified peaks should be removed. This code is only sufficient for analyzing 5 amino acids: alanine, valine, norleucine, glutamic acid and phenylalanine. 
