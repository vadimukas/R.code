# ANOVA in R
# https://statsandr.com/blog/anova-in-r/

# also https://statsandr.com/blog/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-better-way/

install.packages("palmerpenguins")
library(palmerpenguins)

library(tidyverse)

dat <- penguins %>%
  select(species, flipper_length_mm)

summary(dat)

library(ggplot2)

ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")+
  theme_test()

# “Are flippers length different for the 3 species of penguins?”.

# first computin ANOVA
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

# next check the normality visually
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

# or formally Shapiro-Wilk test

shapiro.test(res_aov$residuals)

#P-value of the Shapiro-Wilk test on the residuals is larger than the usual significance level of 
# α = 5 % , so we do not reject the hypothesis that residuals follow a normal distribution (p-value = 0.261).

# Aslo Boxplot and dotplots
par(mfrow = c(1, 1))
boxplot(flipper_length_mm ~ species,
        data = dat
)

library("lattice")
dotplot (flipper_length_mm ~ species,
           data = dat
)

# # Levene's test
library(car)

leveneTest(flipper_length_mm ~ species,
           data = dat
)

# The p-value being larger than the significance level of 0.05, we do not reject the null hypothesis,
# so we cannot reject the hypothesis that variances are equal between species (p-value = 0.719).


