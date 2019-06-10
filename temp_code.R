# to write R output table to txt file
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

# fasttime packages to import data into R
library(fasttime)
fastPOSIXct("2003-02-27")

# farst_strtime() vs lubridate
library(lubridate)
parse_date_time(x, order = "ymd")

library(fasttime)
fast_strptime(x, format = "%Y-%m-%d")
library(fasttime)
# Examine structure of dates
str(dates)
# Use fastPOSIXct() to parse dates
fastPOSIXct(dates) %>% str(dates)

# export datetimes
library(tidyverse)
akl_hourly %>%
  select(datetime) %>%
  write_csv("tmp.csv")

# formatting datetimes
library(lubridate)
mystamp<-stamp("Tuesday October 10 2017")

#using fast_srttime() for lubridate
# Head of dates
head(dates)

# Parse dates with fast_strptime for "2015-12-31T11:00:00Z" 
fast_strptime(dates, 
format = "%Y-%m-%dT%H:%M:%SZ") %>% str(dates)

#How to plot Air Pollution Data with Openair 
# https://www.r-exercises.com/2018/03/05/how-to-plot-with-air-pollution-data-openair/ 
# tutorial will cover the following openair functions:

# SummaryPlot()
# windRose()
# pollutionRose()
# percentileRose()
# timePlot()

# install devtools
# install region5air using devtools
# calendarPlot()
devtools::install_github("NateByers/region5air")

# load region5air and chicago_air dataframe
library(region5air)
data(chicago_air)
head(chicago_air)

# the class of that “date” column is character, We need to change it using the as.POSIXct() function. 
chicago_air$date <- as.POSIXct(chicago_air$date, tz = "America/Chicago")

# Now we just feed the first four columns of the data frame to the summaryPlot() function. 
# We use the select() function from dplyr to select our columns and we use the short hand date
# “solar” to select the “date” column, the “solar” column, and all the columns in between.

library(openair)
library(dplyr)

summaryPlot(select(chicago_air, date:solar))

data(chicago_wind)
head(chicago_wind)

# We need to create a “date” column with a POSIXct class, so we’ll use the as.POSIXct() function again. 
# This time we will need to provide information in the format parameter.

chicago_wind$datetime <- as.POSIXct(chicago_wind$datetime, format ="%Y%m%dT%H%M",tz = "America/Chicago")

# Now we’ll rename the columns using the rename() function from dplyr
chicago_wind <- rename(chicago_wind, date = datetime, ws = wind_speed, wd = wind_dir) 

# We have prepared the dates and column names above so that now 
# we can feed the data frame to the windRose() function.

windRose(chicago_wind) # default is m/s

#We can split the data frame by time periods by using the type argument.

windRose(chicago_wind, type = "weekday")

pollutionRose(chicago_wind, pollutant = "ozone", # we can use the breaks parameter
breaks = c(0, .02, .04, .06, .07, .08)) # to create our own breakpoints

#We can also look at the values by time periods.
pollutionRose(chicago_wind, pollutant = "ozone", type = "month")

# The percentileRose() function calculates percentile levels of a pollutant 
#and plots them by wind direction. This can help you quickly visually identify potential sources by wind direction.
percentileRose(chicago_wind, pollutant = "ozone", smooth =TRUE)

#time series plots can be easily produced using timePlot().
timePlot(chicago_air, pollutant = c("zone", "temp", "solar"))

# calendarPlot() displays daily values in a calendar format.
calendarPlot(chicago_air, pollutant = "ozone")
calendarPlot(chicago_wind, pollutant = "ozone", annotate = "ws")

# cleaning data
# missing values (NA)
is.na()
# is there is any missing value in dataframe
any(is.na())
# to see which rows have no missing values
complete.cases()
# to remove rows with missing values
na.omit()
# Replace all empty strings in status with NA
#data$string [data$string == ""] <- NA

#How to save (and load) datasets in R: An overview
#https://www.r-bloggers.com/how-to-save-and-load-datasets-in-r-an-overview/
data <- read.table("https://vincentarelbundock.github.io/Rdatasets/csv/carData/MplsStops.csv",
                   sep = ",", header = T,
                   row.names = 1)
scroll_box(kable(head(data), row.names = F),
           width = "100%", height = "300px")
View(data)

# I’m assigning a new column data$gender.not.known which is TRUE whenever data$gender is "Unknown" or NA
data$gender.not.known <- is.na(data$gender) | data$gender == "Unknown"

#saving your dataset
# Option 1.1: save()
save(data, file = "data.Rdata")
# if not compressing the data (faster) add compress=F 
save(data, file = "data.Rdata",compress=F)
# to load dataset
load(file = "data.Rdata")
# to save dataset with a different name (2 dataframes)
data2 <- data
save(list = c("data", "data2"), file = "data.Rdata")
#Option 1.2: saveRDS() - one object in one file
saveRDS(data, file = "data.Rds")
data.copy <- readRDS(file = "data.Rds")
#Option 2: Save as a CSV file
# Option 2.1: write.table()
write.table(data, file = "data.csv",
            sep = "\t", row.names = F)
# Option 2.2: fwrite() for very big datasets
library(data.table)
fwrite(data,
       file = "fwrite-data.csv",
       sep = "\t")
# Option 3: Save as an Excel file
library(WriteXLS)
WriteXLS(data, ExcelFileName = "data.xlsx",
         SheetNames = "my data",
         AdjWidth = T,
         BoldHeaderRow = T)