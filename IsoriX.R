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
# First, load the toy data - Germany's GNIP + GNIPDataEU
head(GNIPDataDE)
#load sample GNIP data for 2H
rawGNIP <- read.csv("D:/R/R.code/Isorix/Wiser_BulkData_Eurasia_2H.csv")
#formating the data for IsoRix
rawGNIP$year.begin  <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%Y"))
rawGNIP$year.end    <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%Y"))
rawGNIP$year.span   <- rawGNIP$year.begin - rawGNIP$year.end
rawGNIP$month.begin <- as.numeric(format(as.Date(rawGNIP$Begin.Of.Period), "%m"))
rawGNIP$month.end   <- as.numeric(format(as.Date(rawGNIP$End.of.Period), "%m"))
rawGNIP$day.span    <- as.Date(rawGNIP$Begin.Of.Period) - as.Date(rawGNIP$End.of.Period)
rawGNIP$Year        <- as.numeric(format(as.Date(rawGNIP$Date), "%Y"))
rawGNIP$Month       <- as.numeric(format(as.Date(rawGNIP$Date), "%m"))
#Second, we identify the rows for which crucial information is missing
rows_missing_info <- is.na(rawGNIP$H2) |
  is.na(rawGNIP$day.span) |
  rawGNIP$day.span > -25 |
  rawGNIP$day.span < -35 
#Third, we only keep the rows and columns we are interested in:
columns_to_keep <- c("Name.of.Site", "Latitude", "Longitude", "Altitude",
                     "Year", "Month", "H2")
GNIPData <- rawGNIP[!rows_missing_info, columns_to_keep]
#Fourth, we turn the variable Name.of.Site into a factor:
GNIPData$Name.of.Site <- as.factor(GNIPData$Name.of.Site)

