# ANOVA in R
# https://statsandr.com/blog/anova-in-r/

# also https://statsandr.com/blog/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-better-way/

install.packages("palmerpenguins")
library(palmerpenguins)

library(tidyverse)

dat <- penguins %>%
  select(species, flipper_length_mm)

summary(dat)
