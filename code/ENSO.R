# Data from: https://psl.noaa.gov/gcos_wgsp/Timeseries/Nino4/
# Reference: Rayner N. A., D. E. Parker, E. B. Horton, C. K. Folland, 
# L. V. Alexander, D. P. Rowell, E. C. Kent, A. Kaplan, Global analyses 
# of sea surface temperature, sea ice, and night marine air temperature 
# since the late nineteenth century, J. Geophys. Res., 108 (D14), 4407, 
# doi:10.1029/2002JD002670, 2003.

# Load data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/Environmental/ENSO.csv")

# Get rid of last column 
data <- data[,-14]

# Convert to long format
data_long <- data %>% 
  pivot_longer(cols = -1, names_to = "Month", values_to = "MEI")

# Average summer months
avg_MEI <- data_long %>% 
  filter(Month %in% c("JJ", "JA", "AS")) %>% 
  group_by(YEAR) %>% 
  summarise(MEI = mean(MEI))

# Plot 
plot(x = avg_MEI$YEAR,
     y = avg_MEI$MEI,
     type ="l")
