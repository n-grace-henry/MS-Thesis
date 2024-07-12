setwd("~/Documents/GitHub/CSIA_lab_work/data")

# Load Packages
library(dplyr)
library(readr)
library(ggplot2)

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


