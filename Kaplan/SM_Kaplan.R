# https://dtkaplan.github.io/SM2-bookdown/ 
# http://www.macalester.edu/~kaplan/ISM/ISM.Rdata 
# https://www.macalester.edu/~kaplan/ISM/

# load the data ISM.RData
load("D:/R/R.code/Kaplan/ISM.RData")

library(tidyverse)

setwd("D:/R/R.code/Kaplan/")
head(swim)
swim$minutes = swim$time /60
swim %>% 
  mutate (minutes = time /60)

women = subset (swim, sex == "F")
woman
write.table(women, file = "swimF.csv", sep = " ")

# Chapter 3 Describing Variation 
# https://dtkaplan.github.io/SM2-bookdown/describing-variation.html 

head(galton)
qdata(63, galton$height)
boxplot(galton$height)
hist(galton$height)
rug (galton$height, ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))
quantile(galton$height)
qqplot(galton$height, y = galton$family)
quantile(galton$height)
plot(density(galton$height), bw = 0.15)
rug (galton$height, ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))

ggplot(galton, aes(y=height, x= sex))+
  geom_boxplot()+
  theme_classic()

ggplot(galton, aes(y=height, x= sex))+
  geom_col()+
  theme_classic

resids = galton$height - mean(galton$height)
sum (resids ^2)

# simple stat graphs

library(lattice)
histogram(galton$height, type = "density")
densityplot(galton$height)
bwplot(galton$height)
bwplot(height ~ sex, data = galton)

# displays of categorical variables

prop.table(table(galton$sex))
barchart(table(galton$sex))


# The language of Models
library(lattice)
utils<-read.csv("D:/R/R.code/Kaplan/utilities.csv")

cps<-read.csv("D:/R/R.code/Kaplan/cps.csv")

xyplot(ccf ~ temp, data = utils)

bwplot(wage ~ sex, data = cps)

bwplot(wage ~ sector, data = cps, scales = list(rot=45))

densityplot(~ wage, groups = sex, data = cps, auto.key = TRUE)

# Multiple explanatory variables 

xyplot(wage ~ age, groups = sex, data = cps, auto.key = TRUE)

# splitting the plot
xyplot(wage ~ age | sex, data = cps)

# 4.7.2 Fitting models and finding model values
swim<-read.csv("D:/R/R.code/Kaplan/swim100m.csv")

mod1=lm(time ~ 1, data = swim)
mod1               
plot(mod1)                 

xyplot(time + fitted(mod1) ~ year, data = swim)

mod2=lm(time ~ 1 + year, data = swim) 
# or 
mod2=lm(time ~ year, data = swim)

mod3=lm(time ~ 1 + sex, data = swim)
mod4=lm(time ~ 1 + sex + year, data = swim)
mod5=lm(time ~ 1 + year + sex+year:sex, data = swim)
# or
mod5<-lm(time ~ year * sex, data = swim)

xyplot(fitted(mod5) + fitted(mod3) ~ year, data = swim, auto.key = TRUE)
xyplot(fitted(mod5) ~ year, data = swim, auto.key = TRUE)

# suppressing the intercepter term

mod6<-lm(time ~ year-1, data = swim)
xyplot(fitted(mod6) ~ year, data = swim, auto.key = TRUE)

mod7<-lm(time ~ year + I(year^2) + sex, data = swim)
xyplot(time + fitted(mod7) ~ year, data = swim, auto.key = TRUE)
