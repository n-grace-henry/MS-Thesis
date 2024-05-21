# Data from: https://psl.noaa.gov/enso/mei/
# Reference: Kobayashi, S., Ota, Y., Harada, Y., Ebita, A., Moriya, M., Onoda, H., 
#Onogi, K., Kamahori, H., Kobayashi, C., Endo, H., Miyaoka, K., Takahashi, K., 2015. 
#The JRA-55 Reanalysis: general specifications and basic characteristics. J. Meteor. 
#Soc. Jpn. 93, 5-48. DOI: 10.2151/jmsj.2015-001

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
