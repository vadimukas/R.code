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

library(lattice)
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

# Another method to test normality and homogeneity

par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances
plot(res_aov, which = 1)

# If homogeneity of variances was violated, the red line would not be flat.

# 2. Normality
plot(res_aov, which = 2)
# normality was violated, points would consistently deviate from the dotted line.

# Preliminary analyses

library(ggplot2)

# boxplots
ggplot(dat) +
  aes(x = species, y = flipper_length_mm) +
  geom_boxplot()+
    theme_test()

# and descriptive stats
aggregate(flipper_length_mm ~ species,
          data = dat,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)  

library(dplyr)

group_by(dat, species) %>%
  summarise(
    mean = mean(flipper_length_mm, na.rm = TRUE),
    sd = sd(flipper_length_mm, na.rm = TRUE)
  )

# ANOVA in R

# With the oneway.test() function:
  # 1st method:
  oneway.test(flipper_length_mm ~ species,
              data = dat,
              var.equal = TRUE # assuming equal variances
  )

# therefore, we can conclude that at least one species is different than the others in terms of 
# flippers length (p-value < 2.2e-16).

# A nice and easy way to report results of an ANOVA in R is with 
# the report() function from the {report} package:  
  
  install.packages("remotes")
  remotes::install_github("easystats/report") # You only need to do that once
  library(report) # Load the package every time you start R
  
  report(res_aov)

#  Results of an ANOVA, however, do NOT tell us which group(s) is(are) different from the others.

# Post-hoc test
  
  # Issue of multiple testing
  # Chinstrap versus Adelie
  # Gentoo vs. Adelie
  # Gentoo vs. Chinstrap
  
   #  The most often used are the Tukey HSD and Dunnett’s tests:
  
  # Tukey HSD test
  # Tukey HSD is used to compare all groups to each other (so all possible comparisons of 2 groups).
  
  install.packages("multicomp")
  library(multcomp)
  
  # Tukey HSD test:
  post_test <- glht(res_aov,
                    linfct = mcp(species = "Tukey")
  )
  
  summary(post_test)
  
  # The results of the post-hoc test can be visualized with the plot() function:
  
  par(mar = c(3, 8, 3, 3))
  plot(post_test)
  
# We see that the confidence intervals do not cross the zero line,
  # which indicate that all groups are significantly different.

# Note that the Tukey HSD test can also be done in R with the TukeyHSD() function:
  
  TukeyHSD(res_aov)

  plot(TukeyHSD(res_aov))  
  
  # Dunnett’s test. Dunnett is used to make comparisons with a reference group. 
  # In R, the Dunnett’s test is done as follows (the only difference with the code for the Tukey HSD test is
  #  in the line linfct = mcp(species = "Dunnett")):
  
  library(multcomp)
  
  # Dunnett's test:
  post_test <- glht(res_aov,
                    linfct = mcp(species = "Dunnett")
  )
  
  summary(post_test)
  
  # The interpretation is the same as for the Tukey HSD test’s except that in the Dunett’s test we only compare:
  
  # Chinstrap versus Adelie (line Chinstrap - Adelie == 0)
  #  Gentoo vs. Adelie (line Gentoo - Adelie == 0)
  
  par(mar = c(3, 8, 3, 3))
  plot(post_test)

   #The reference category can be changed with the relevel() function (or with the {questionr} addin)
  
  # Change reference category:
  dat$species <- relevel(dat$species, ref = "Gentoo")
  
  # Check that Gentoo is the reference category:
  levels(dat$species)
  
  res_aov2 <- aov(flipper_length_mm ~ species,
                  data = dat
  )
  # Dunnett's test:
  post_test <- glht(res_aov2,
                    linfct = mcp(species = "Dunnett")
  )
  
  summary(post_test)

  par(mar = c(3, 8, 3, 3))
  plot(post_test)  

# Other p-values adjustment methods
  # For the interested readers, note that you can use other p-values adjustment methods 
  # by using the pairwise.t.test() function:
    
    pairwise.t.test(dat$flipper_length_mm, dat$species,
                    p.adjust.method = "holm"
    )  

  # Visualization of ANOVA and post-hoc tests on the same plot
    # Edit from here
    x <- which(names(dat) == "species") # name of grouping variable
    y <- which(
      names(dat) == "flipper_length_mm" # names of variables to test
    )
    method1 <- "anova" # one of "anova" or "kruskal.test"
    method2 <- "t.test" # one of "wilcox.test" or "t.test"
    my_comparisons <- list(c("Chinstrap", "Adelie"), c("Gentoo", "Adelie"), c("Gentoo", "Chinstrap")) # comparisons for post-hoc tests
    # Edit until here
    
    # Edit at your own risk
    library(ggpubr)
    for (i in y) {
      for (j in x) {
        p <- ggboxplot(dat,
                       x = colnames(dat[j]), y = colnames(dat[i]),
                       color = colnames(dat[j]),
                       legend = "none",
                       palette = "npg",
                       add = "jitter"
        )
        print(
          p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                                 method = method1, label.y = max(dat[, i], na.rm = TRUE)
          )
          + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
        )
      }
    }
    