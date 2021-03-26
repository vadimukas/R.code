# https://dtkaplan.github.io/SM2-bookdown/ 
# http://www.macalester.edu/~kaplan/ISM/ISM.Rdata 
# https://www.macalester.edu/~kaplan/ISM/

# load the data ISM.RData
load("D:/R/R.code/Kaplan/ISM.RData")a

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
