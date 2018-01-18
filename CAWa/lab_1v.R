
#R as a pocket calculator

6+7

# variables in R

a<-(-7) # set a to 5
b<-2 
a+b
c<-a+b

v<-c(2,3,5,6)

# use simple functions in R
min (v)
max (v)
mean (v)
plot (v)
v2<-c(6,5,3,2)
plot(v2, type="l")
cor(v,v2)
my_table<-data.frame(v,v2)
head(table)

#working with real data

temp<-read.table("data_temp.txt", header = T)
head (temp)
min (temp)
max(temp[,3])
max(temp[1,-(1:2)]
max(temp$Almaty)
plot(temp[,3], type="l",main="Temp Almaty")
plot(temp$Almaty, type="l", main = "Tair Almaty")
plot(temp$Almaty, type="l", main = "Tair Almaty", ylab = 'T,C', xlab = 'date')
# date information
ISOdate(1901,1,1)
date<-ISOdate(temp$year,temp$month,1)
date
plot(date, temp$Almaty, type="l", main = "Tair Almaty", ylab = 'T,C', xlab = 'date', col='red', ylim = c(-30,30))

# calculation of annual data

temp_annual<-aggregate(temp, by=list(temp$year), FUN = mean)
head(temp_annual)
plot(temp_annual$year, temp_annual$Almaty, type="l")
plot(temp_annual$year, temp_annual$Almaty, type="l", main = "Tair Almaty", ylab = 'T,C', xlab = 'date', col='blue', ylim = c(5,20))
plot(temp_annual$year, temp_annual$Termez, type="l", main = "Tair Termez", ylab = 'T,C', xlab = 'year', col='green', ylim = c(15,20))
lines(temp_annual$year, temp_annual$Termez, col='green')
cor(temp_annual$Almaty,temp_annual$Termez)

# work with precipitation data

precip<-read.table("data_prec.txt", header = T)
precip_annual<-aggregate(precip, by=list(precip$year), FUN = sum)
head(precip_annual)
plot(precip_annual$Group.1, precip_annual$Almaty, type="l", main = "Precipitation Almaty & Termez", ylab = 'T,C', xlab = 'date', col='blue', ylim = c(0,900))
lines(precip_annual$Group.1, precip_annual$Termez, col='green')
cor(precip_annual$Almaty,precip_annual$Termez)

# Monthly Means of Temperature and Precipitation 

temp_monthly_means<-aggregate(temp,by=list(temp$month), FUN=mean)
head(temp_monthly_means)
plot(temp_monthly_means$month, temp_monthly_means$Almaty, type="l", main = "Monthly Temperature and Prepitation Almaty", ylab = 'T,C/P,mm', xlab = 'month', col='red', ylim = c(-10,150))
precip_monthly_means<-aggregate(precip,by=list(precip$month), FUN=mean)
head(precip_monthly_means)
lines(precip_monthly_means$month, precip_monthly_means$Almaty, col='blue')

precip_monthly_means<-aggregate(precip,by=list(precip$month), FUN=mean)
head(precip_monthly_means)
plot(precip_monthly_means$month, precip_monthly_means$Almaty, type="l", main = "Monthly Prepitation Almaty & Termez", ylab = 'P,mm', xlab = 'month', col='blue', ylim = c(0,120))
lines(precip_monthly_means$month, precip_monthly_means$Termez, col='Red')

temp_monthly_means<-aggregate(temp,by=list(temp$month), FUN=mean)
head(temp_monthly_means)
plot(temp_monthly_means$month, temp_monthly_means$Almaty, type="l", main = "Monthly Temperature Almaty & Termez", ylab = 'T,C', xlab = 'month', col='black', ylim = c(-10,40))
lines(temp_monthly_means$month, temp_monthly_means$Termez, col='Red')
write.table(temp_annual,'temp.annual.txt', col.names = TRUE, row.names = F,sep = '\t', quote=F,)
write.table(precip_annual,'precip.annual.txt', col.names = TRUE, row.names = F,sep = '\t', quote=F,)

# Linear regression models

plot(temp_annual$year, temp_annual$Almaty, type="l", main = "Tair Almaty", ylab = 'T,C', xlab = 'date', col='red', ylim = c(5,10))
trend_lm<-lm(temp_annual$Almaty~temp_annual$year)
trend_lm
abline (trend_lm, col="black")
temp_annual_subset<-subset(temp_annual,year>1950)
head(temp_annual_subset)
trend_lm_subset<-lm(temp_annual_subset$Almaty~temp_annual_subset$year)
trend_lm_subset
abline(trend_lm_subset,col="green")

# Sen's slope
library(zyp)
x<-temp_annual$year
y<-temp_annual$Almaty
sen_slope<-zyp.sen(y~x)
sen_slope

#sen$slopes
abline(sen_slope$coefficients[1], sen_slope$coefficients[2], col="violet")

#sen$slope for subset
# Sen's slope
library(zyp)
x<-temp_annual_subset$year
y<-temp_annual_subset$Almaty
sen_slope<-zyp.sen(y~x)
sen_slope

#sen$slopes
abline(sen_slope$coefficients[1], sen_slope$coefficients[2], col="cyan")

# Kendall significance testing

t.test
periode1<-subset(temp_annual, year<1950)
periode2<-subset(temp_annual, year>=1950)
t.test(periode1$Almaty, periode2$Almaty, var.equal=T, alternative="greater")
abline(periode1$Almaty, periode2$Almaty, col="yellow")

MannKendall(temp_annual$Almaty)
MannKendall(temp_annual_subset$Almaty)