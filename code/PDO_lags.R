setwd("~/Documents/GitHub/CSIA_lab_work/data")

# Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Load Data
PDO <- read.csv(file = "Environmental/PDO.csv")

# Convert PDO to tidy format 
PDO_long <- gather(PDO, Month, Value, -Year)

# Sort by year
PDO_long <- arrange(PDO_long, Year)

# Convert month to date
PDO_long <- PDO_long %>% 
  mutate(month_number = match(Month, month.abb))

# Plot PDO from 1965-2022
PDO_subset <- subset(PDO_long, Year >= 1965 & Year <= 2022)
PDO_annual <- PDO_subset %>%
  group_by(Year) %>%
  summarise(Value = mean(Value, na.rm = TRUE))

plot(x = PDO_annual$Year,
     y = PDO_annual$Value,
     type = "l",
     xlab = "Year",
     ylab = "PDO")

ggplot(PDO_annual, aes(x = Year, y = Value, fill = Value > 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_col() + 
  scale_fill_manual(values = c("TRUE" = "red2", "FALSE" = "blue3")) +
  labs(title = "PDO Index",
       x = "Year",
       y = "PDO") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"), 
    text = element_text(family = "Times New Roman") 
  )



# Write csv of tidy PDO data
write.csv(PDO_annual, "~/Documents/GitHub/CSIA_lab_work/data/environmental/PDO_tidy.csv")


