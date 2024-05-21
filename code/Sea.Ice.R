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


# Data source: Walsh, J. E., W. L. Chapman, F. Fetterer, and J. S. Stewart. (2019). 
#Gridded Monthly Sea Ice Extent and Concentration, 1850 Onward, Version 2 [Data Set]. 
#Boulder, Colorado USA. National Snow and Ice Data Center. 
#https://doi.org/10.7265/jj4s-tq79. Date Accessed 05-21-2024.
# Load data 
sea_ice <- read_csv("~/Documents/GitHub/CSIA_lab_work/data/Environmental/sibt_areas_v2.csv")

# Create year column
year.2digit <- substr(sample_df$group_id, 1, 2) #none of this is updated, only copy pasted

year <- vector(mode="character")
for(i in 1:length(year.2digit)){
  if(year.2digit[i] <= 22){
    year[i] <- paste0(20, year.2digit[i])
  } else{
    year[i] <- paste0(19, year.2digit[i])
  }
}

