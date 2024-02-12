NPGO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/Environmental/NPGO.csv")
PDO <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/Environmental/PDO.csv")

avg.PDO <- vector(length = 170)
for(i in length(avg.PDO)){
  mean(as.numeric(PDO[1, 2:13]))
  
}
dat_yr <- ts(PDO,
             start = 1991, end = 2020,
             frequency = 1)
plot.ts(dat_yr)

mean(as.numeric(PDO[1, 2:13]))