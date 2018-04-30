Lqq# to write R output table to txt file
write.table(dataframename, file = 'filename', sep = ',')

## Plotting the CRU time series for CRU!
plot(date,pet_shuchinsk, type="l")

## Simple Functions for grids
sum_pet<-sum(pet)
plot(sum_pet)
plot(world_map, add=T)

world_map<-readShapePoly("GIS data/TM_WORLD_BORDERS-0.2")
plot(world_map, add=T)
plot(pet)
plot(world_map, add=T)

# coordinates Kokshetau 69.25, 53.25 
# coordinates Suchinsk 70.25, 52.75

par(mfrow = c(1, 1))
lines(TaSh$X1, xlab = "date", ylab = "Ta", col="blue")
lines(TaSh$X2, xlab = "date", ylab = "Ta", col="red")

<<<<<<< HEAD
# Is it time to ditch the Comparison of Means (T) Test?

#To download data in R, use the following syntax:
url = "https://sites.google.com/site/statsr4us/intro/software/rcmdr-1/TeachingRatings.rda"
download.file(url,"TeachingRatings.rda")
load("TeachingRatings.rda")
#For equal variances, the following syntax is used for T-test and the OLS regression model
t.test(beauty ~ gender, var.equal=T, data=TeachingRatings)
lm<-lm(beauty ~ gender, data=TeachingRatings)
summary(lm)
plot(beauty ~ gender, data=TeachingRatings)
#For unequal variances, we need to install and load the nlme package to run a gls version 
#of the variance weighted least square Regression model.
t.test(beauty ~ gender, data=TeachingRatings)
library(nlme)
gls<-gls(beauty ~ gender,  weights=varIdent(form = ~ 1 | gender),data=TeachingRatings)
summary(gls)

#check local time
Sys.timezone()
Sys.Date()

