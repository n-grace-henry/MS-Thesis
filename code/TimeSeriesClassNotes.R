#there is a time series function ts()
#you pass the data and when the time series starts and ends

dat_1 <- rnorm(30) #to deal with missing values, edit vector to add NAs
dat_yr <- ts(dat_1,
             start = 1991, end = 2020,
             frequency = 1)

plot.ts(dat_yr)
