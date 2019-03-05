# install dependencies
install.packages("IsoriX", dependencies = TRUE)
#load Isorix package
library(IsoriX)
# online book https://bookdown.org/content/782/
#set working directory 
setwd("D:/R/R.code/Isorix")
# setting global options for isoriX
options_IsoriX(example_maxtime = Inf)
options_IsoriX(Ncpu = 4) 
# load the toy data - Germany's GNIP
head(GNIPDataDE)