# Data from Fetterer, F., K. Knowles, W. N. Meier, M. Savoie, and A. K. 
#Windnagel. (2017). Sea Ice Index, Version 3 [Data Set]. Boulder, Colorado USA. 
#National Snow and Ice Data Center. https://doi.org/10.7265/N5K072F8. 
#Date Accessed 05-21-2024.

# Load all csv files into one df
df <- list.files(path=setwd("~/Documents/GitHub/CSIA_lab_work/data/Environmental/Sea.Ice")) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# Plot July extent only 
jul <- df[df$mo == 6, ]

plot(x = jul$year,
     y = jul$area)

# Plot August extent only 
aug <- df[df$mo == 7, ]

plot(x = aug$year,
     y = aug$area)

# Write new csv from df
file.name <- "~/Documents/GitHub/CSIA_lab_work/data/Environmental/sea_ice.csv"
write.csv(df, file = file.name)

# Data source: Walsh, J. E., W. L. Chapman, F. Fetterer, and J. S. Stewart. (2019). 
#Gridded Monthly Sea Ice Extent and Concentration, 1850 Onward, Version 2 [Data Set]. 
#Boulder, Colorado USA. National Snow and Ice Data Center. 
#https://doi.org/10.7265/jj4s-tq79. Date Accessed 05-21-2024.
# Load data 
sea_ice <- read_csv("~/Documents/GitHub/CSIA_lab_work/data/Environmental/sibt_areas_v2.csv")

# Only keep areas columns of interest
sea_ice <- sea_ice[-c(1,2), c(1, 2, 14)]

# Create year column
names(sea_ice) <- c("date", "N_hem", "Bering_Sea")
year <- substr(sea_ice$date, 1, 4) 

sea_ice$year <- year

# Only keep years 1965 and above
sea_ice <- sea_ice[sea_ice$year >= 1965, ]

# Create month column 
month <- rep(seq(1, 12), length.out = nrow(sea_ice))

sea_ice$month <- month

# Plot Bering Sea ice extent for July 
september <- sea_ice[sea_ice$month == 9, ]

plot(x = september$year,
     y = september$Bering_Sea)



