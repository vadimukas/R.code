# https://www.r-bloggers.com/quick-way-of-installing-all-your-old-r-libraries-on-a-new-device/
# step 1: Save a list of packages installed in your old computing device (from your old device).

setwd ("D:/R/")
installed <- as.data.frame(installed.packages())
write.csv(installed, 'installed_previously.csv')

#Step 2: Create a list of libraries from your old list that were not already installed when you
# freshly download R (from your new device).

installedPreviously <- read.csv('installed_previously.csv')
baseR <- as.data.frame(installed.packages())
toInstall <- setdiff(installedPreviously, baseR)

# Step 3: Download this list of libraries.

install.packages(toInstall)

# another protocol 

# first install Rtools by downloading them off-site exec
# config Rtools
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# restart Rstudio and
Sys.which("make") 

# # run on old computer / r version
setwd("D:/Dropbox/Temp/Rpackages/") # or any other existing temp directory
packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages")

# run on new computer / r version
setwd("D:/Dropbox/Temp/Rpackages/") # or any other existing temp directory
load("Rpackages")
for (p in setdiff(packages, installed.packages()[,"Package"]))
  install.packages(p)

