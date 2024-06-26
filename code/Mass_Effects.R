setwd("~/Documents/GitHub/CSIA_lab_work/data")

# Load Packages
library(dplyr)
library(readr)
library(ggplot2)

#### Drift correct and do not consolidate ####
name <- c("Analysis", "ID1", "RT", "AreaAll", "d29N", "d15N", "AAID") 

# STD values
ALA <- -1.61 
VAL <- -0.67 
NOR <- 14.485 
PHE <- -5.004
GLU <- -3.042

# Read in NACHO csv and set output file name
data.1 <- SL.1 <- read.csv("cleaned/20231117_GHenry_CSIA.csv")
colnames(data.1)<- name
file.name <- "processed/20231117.csv" #file name for output file including relative file path

# Correct to international standard of N air
#Calculations of offset values were done in R script "Correct_to_Nair.R"
#Three EA runs were looked at, the second was chosen as the most representative to base corrections off
#No linear relationship was found between offset and measured value so one average value
#will be applied to raw data
#The offset values were calculated as EA measured d15N - reference 
offset <- mean(c(0.40160, 0.47160, 0.41725))

data.1$d15N.correct <- data.1$d15N - offset

# Linear Model for Drift Correction
#Fit a linear model to your external standards with "Analysis" (injection number) as the dependent variable and 
#d15N as the response variable

data.1STD <- subset(data.1, ID1=="5AA") #get only the standard data 
AA <- unique(unlist(data.1STD$AAID)) #make a list of the AAs in the data

Intercept<-data.frame(Intercept=rep(NA,length(AA))) #initiate a dataframe for the intercepts of the linear model
for(i in 1:length(AA)){
  data <- subset(data.1STD, AAID==AA[i])
  Intercept[i,1]<- coef(summary(lm(as.numeric(d15N.correct)~as.numeric(Analysis), data=data)))[1,1]
}
Intercept #intercept values looped by aa

Slope<-data.frame(Slope=rep(NA,length(AA))) #initiate a dataframe for the slopes of the linear model
for(i in 1:length(AA)){
  data <- subset(data.1STD, AAID==AA[i])
  Slope[i,1]<- coef(summary(lm(as.numeric(d15N.correct)~as.numeric(Analysis), data=data)))[2,1]
}
Slope #slope values looped by aa

Coef<- data.frame(AA, Intercept, Slope) #creating a dataframe of the slope and intercepts values for each AA

# Adding Coefs and Standard Values to the dataset

actual <- ifelse(data.1$AAID=="NOR", NOR, 
                 ifelse(data.1$AAID=="ALA", ALA,
                        ifelse(data.1$AAID=="VAL", VAL,
                               ifelse(data.1$AAID=="PHE", PHE,
                                      ifelse(data.1$AAID=="GLU", GLU,0)))))

actual #check your data -- if there are 0s than you have an AA that is not included in the standard 12AA mix and the code will need
#to be modified accordingly. Data should only be used for AAs that are included in the external standard or standards that 
#do not have a substantial drift (check your d15N verse Analysis plot to verify if the AA is drifting)

slope <- ifelse(data.1$AAID=="NOR", filter(Coef, AA=="NOR")[1,3], 
                ifelse(data.1$AAID=="ALA", filter(Coef, AA=="ALA")[1,3],
                       ifelse(data.1$AAID=="VAL", filter(Coef, AA=="VAL")[1,3],
                              ifelse(data.1$AAID=="GLU", filter(Coef, AA=="GLU")[1,3],
                                     ifelse(data.1$AAID=="PHE", filter(Coef, AA=="PHE")[1,3], 0)))))

intercept <-   ifelse(data.1$AAID=="NOR", filter(Coef, AA=="NOR")[1,2], 
                      ifelse(data.1$AAID=="ALA", filter(Coef, AA=="ALA")[1,2],
                             ifelse(data.1$AAID=="VAL", filter(Coef, AA=="VAL")[1,2],
                                    ifelse(data.1$AAID=="GLU", filter(Coef, AA=="GLU")[1,2],
                                           ifelse(data.1$AAID=="PHE", filter(Coef, AA=="PHE")[1,2], 0)))))

# Applying Drift Correction 
difference <- actual-(data.1$Analysis*slope+intercept) #Applying both a drift and step correction in on estep from linear model data
adj <- data.1$d15N.correct + difference
data <- cbind(data.1, adj)

# Write csv file 
write.csv(data, file = file.name)


#### Mass vs d15N ####

# compile processed folder into one sheet 
df <- list.files(path=setwd("~/Documents/GitHub/CSIA_lab_work/data/processed")) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# write csv of new data 
write.csv(df, file = "final/data_full.csv")

# read in new data file 
setwd("~/Documents/GitHub/CSIA_lab_work/data")
data <- read.csv(file = "final/data_full.csv")

# plot areas vs d15N for PHE only, removing STDs
phe.no.std <- data[data$AAID == "PHE" &
            !data$ID1 == "5AA",]

lm <- lm(adj ~ AreaAll, data = phe.no.std)
summary(lm)
plot(x = phe.no.std$AreaAll,
     y = phe.no.std$adj,
     xlab = "Area",
     ylab = "d15N",
     main = "PHE"
)
abline(lm)

# plot only phe standards 
phe.std <- data[data$AAID == "PHE" &
            data$ID1 == "5AA",]

lm <- lm(adj ~ AreaAll, data = phe.std)
summary(lm)
plot(x = phe.std$AreaAll,
     y = phe.std$adj,
     xlab = "Area",
     ylab = "d15N",
     main = "PHE - Standards"
)
abline(lm)

# Remove the outliers from the phe data set 
phe_no_out <- phe.no.std[phe.no.std$adj < 10,]

lm_no_out <- lm(AreaAll ~ adj, data = phe_no_out)
summary(lm_no_out)
plot(x = phe_no_out$adj,
     y = phe_no_out$AreaAll,
     xlab = "d15N",
     ylab = "Area",
)
abline(lm_no_out)

# Look at phe outlier values 
outliers <- phe.no.std[phe.no.std$adj > 10,]
(outliers)

# plot area vs glu, removing standards
glu.no.std <- data[data$AAID == "GLU" &
            !data$ID1 == "5AA",]

lm <- lm(adj ~ AreaAll, data = glu.no.std)
summary(lm)
plot(x = glu.no.std$AreaAll,
     y = glu.no.std$adj,
     xlab = "Area",
     ylab = "d15N",
     main = "GLU"
)
abline(lm)

# save excel file to find constants for polynomial fit
library(openxlsx)
excel_file <- "output_file.xlsx"
write.xlsx(glu.no.std, file = excel_file)

# Look to see if low area points are random across the time series 
lowArea <- data[data$AAID == "GLU" &
                  !data$ID1 == "5AA" &
                  data$AreaAll <= 6,]
# Add year column
year.2digit <- substr(lowArea$ID1, 1, 2)
year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}
lowArea$Year <- year

plot(x = lowArea$Year,
     y = lowArea$AreaAll,
     xlab = "Year",
     ylab = "Area",)

# plot area vs glu, standards only
glu.std <- data[data$AAID == "GLU" &
            data$ID1 == "5AA",]

lm <- lm(adj ~ AreaAll, data = glu.std)
summary(lm)
plot(x = glu.std$AreaAll,
     y = glu.std$adj,
     xlab = "Area",
     ylab = "d15N",
     main = "GLU - Standards"
)
abline(lm)

# Mass correct GLU data
a <- 5.980428588 # best fit values found from excel calculations
N <- -0.753450356896991
b <- 23.79092157

# Fit model to data 
predicted <- -a * glu.no.std$AreaAll^N + b

# Difference from predicted
pred.diff <- b - predicted

# Corrected
correct <- glu.no.std$adj + pred.diff

# Plot 
plot(x = glu.no.std$AreaAll,
     y = correct,
     xlab = "Area",
     ylab = "d15N",
     main = "GLU - Corrected"
)

# Add correction to csv and save as new data file 
glu.no.std$adj <- correct
final.masscorrected <- rbind(phe_no_out, glu.no.std)

# Write new csv with no mass outliers 
write.csv(final.masscorrected, file = "final/mass_correct.csv")


