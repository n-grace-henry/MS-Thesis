# Load packages
library(dplyr)
library(readr)
library(MARSS)
library(reshape2)

# Read in data
data <- read.csv(file = "~/Documents/GitHub/CSIA_lab_work/data/final/all_correct_final.csv")
head(data)
yy <- data %>% filter(Age == 2)%>%select(Year,
                      PHE.mean,# Change this per response variable
                      System)

head(yy)
yy%>% ggplot(aes(x = Year, y = PHE.mean, color = System)) + geom_line()+facet_wrap(~System)
dat <- reshape2::acast(yy, System ~ Year, value.var = "PHE.mean")# Change this per response variable

#Change this per response variable
dat.log <- log(dat) #NO3mgL/NO3yield
#dat.log <- dat #d15N/d18O 

#Z-score the data
dat.z <- zscore(dat.log)

Z.models <- list(
  #one hidden state/trend
  H1 = matrix(1,3,1),
  #trends are defined by watershed
  H2 = factor(c("E", "W", "K")))

names(Z.models) <- c("basin","watershed")


mod.list = list(
  #Z = "identity",
  B = "identity",
  U = "unequal",
  A = "zero",
  R = "diagonal and equal",
  x0 = "unequal",
  #Q = "unconstrained",
  tinitx = 0 )

fit = MARSS(dat.z, model=mod.list,
            silent=TRUE, control=list(maxit=6000))

Q.models2 <- c("diagonal and equal", "diagonal and unequal","equalvarcov", "unconstrained")


# SECTION 4: RUN MARSS MODELS ---------------------------------------------

out.tab <- NULL
fits <- list()
for(i in 1:length(Z.models)){
  
  for(Q.model in Q.models2){
    fit.model = c(list(Z=Z.models[[i]], Q=Q.model), mod.list)
    fit = MARSS(dat.z, model=fit.model,
                silent=TRUE, control=list(maxit=6000))
    out=data.frame(model_Z=names(Z.models)[i], 
                   trend_num=length(unique(Z.models[[i]])),
                   Q=Q.model,
                   logLik=fit$logLik, AICc=fit$AICc, 
                   num.param=fit$num.params,
                   num.iter=fit$numIter, converged=!fit$convergence,
                   stringsAsFactors = FALSE)
    out.tab=rbind(out.tab,out)
    fits=c(fits,list(fit))
    
  }
}

min.AICc <- order(out.tab$AICc)
out.tab.1 <- out.tab[min.AICc, ]
out.tab.1 <- cbind(out.tab.1,delta.AICc = out.tab.1$AICc - out.tab.1$AICc[1])
out.tab.1

plot(fits[[4]])
