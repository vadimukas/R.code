# load package open air and lubridate
library(openair)
library(lubridate)
library(methods)

setwd()
# Read prepared wind data file
data.davis <- read.delim('wd_davis_2016P.txt')

# Display the start of the data file
head(data.davis)

# Calculate the u and v wind components
data.davis$u.wind <- data.davis$ws * sin(2 * pi * data.davis$wd/360)
data.davis$v.wind <- data.davis$ws * cos(2 * pi * data.davis$wd/360)

#Display three rows of only the wind data
head(subset(data.davis, select = c(date, ws, wd, u.wind, v.wind)), 5)

# Drop the wind vectors because timeAverage computes them internally
data.davis <- subset(data.davis, select = -c(u.wind, v.wind))

# Daily averages
data.davis.day <- timeAverage(data.davis, avg.time = 'day')
head(data.davis.day, 5)

# write dataframe to file
write.table(data.davis.day, file = 'data.davis.day.2016', sep = ',') 

# read and plot windroses in R 2014
# Read prepared raw wind data file and format to POSIXCT
data.davis2014 <- read.delim("D:/R/R.code/Wind Averaging/wd_davis_2014P.txt")
Sys.timezone()
data.davis2014$date<-as.POSIXct(data.davis2014$date, tz="Asia/Dhaka")
tz(data.davis2014$date)
is.POSIXct(data.davis2014$date)
View(data.davis2014)

# Make daily averages 2014
data.davis2014d <- timeAverage(data.davis2014, avg.time = 'day')
head(data.davis2014d, 5)
View(data.davis2014d)

# write dataframe to file
write.table(data.davis2014d, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2014d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.davis2014d, breaks = 6)
# plot wind rose monthly
windRose(data.davis2014d, type="month", breaks = 6)

# do the same for 2016 AWS Davis

# read and plot windroses in R 2016
data.davis2016 <- read.delim("D:/R/R.code/Wind Averaging/wd_davis_2016P.txt")
Sys.timezone()
data.davis2016$date
data.davis2016$date<-as.POSIXct(data.davis2016$date, tz="Asia/Dhaka")
tz(data.davis2016$date)
is.POSIXct(data.davis2016$date)
View(data.davis2016)

# Make daily averages 2016
data.davis2016d <- timeAverage(data.davis2016, avg.time = 'day')
head(data.davis2016d, 5)
View(data.davis2016d)

# write dataframe to file
write.table(data.davis2016d, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2016d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.davis2016d, breaks = 6)
# plot wind rose monthly
windRose(data.davis2016d, type="month", breaks = 6)

# do the same for 2016 Burabay new
# read and plot windroses in R 2016
data.burabay2016 <- read.delim("D:/R/R.code/Wind Averaging/wind.dir.burabay.daily.2016P.txt")
Sys.timezone()
library(lubridate)
data.burabay2016$date<-parse_date_time(data.burabay2016$date, order = "dmy")
is.POSIXct(data.burabay2016$date)
data.burabay2016$date<-as.POSIXct(data.burabay2016$date, tz="Asia/Dhaka")
tz(data.burabay2016$date)
View(data.burabay2016)

# Make daily averages 2016
data.burabay2016 <- timeAverage(data.burabay2016, avg.time = 'day')
head(data.burabay2016, 5)
View(data.burabay2016)

# write dataframe to file
write.table(data.burabay2016, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2016d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.burabay2016, breaks = 6)
# plot wind rose monthly
windRose(data.burabay2016, type="month", breaks = 6)

# read and plot windroses in R 2015
# Read prepared raw wind data file and format to POSIXCT
data.davis2015 <- read.delim("D:/R/R.code/Wind Averaging/wd_davis_2015P.txt")
Sys.timezone()
data.davis2015$date<-as.POSIXct(data.davis2015$date, tz="Asia/Dhaka")
tz(data.davis2015$date)
is.POSIXct(data.davis2015$date)
View(data.davis2015)

# Make daily averages 2015
data.davis2015d <- timeAverage(data.davis2015, avg.time = 'day')
head(data.davis2015d, 5)
View(data.davis2015d)

# write dataframe to file
write.table(data.davis2015d, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2015d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.davis2015d, breaks = 6)
# plot wind rose monthly
windRose(data.davis2015d, type="month", breaks = 6)

# do the same for 2013 Burabay new
# read and plot windroses in R 2013
data.burabay2013 <- read.delim("D:/R/R.code/Wind Averaging/wind.dir.burabay.daily.2013P.txt")
Sys.timezone()
library(lubridate)
data.burabay2013$date<-parse_date_time(data.burabay2013$date, order = "dmy")
is.POSIXct(data.burabay2013$date)
data.burabay2013$date<-as.POSIXct(data.burabay2013$date, tz="Asia/Dhaka")
tz(data.burabay2013$date)
View(data.burabay2013)

# Make daily averages 2013
data.burabay2013 <- timeAverage(data.burabay2013, avg.time = 'day')
head(data.burabay2013, 5)
View(data.burabay2013)

# write dataframe to file
write.table(data.burabay2013, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2013d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.burabay2013, breaks = 6)
# plot wind rose monthly
windRose(data.burabay2013, type="month", breaks = 6)

# do the same for 2014 Burabay new
# read and plot windroses in R 2014
data.burabay2014 <- read.delim("D:/R/R.code/Wind Averaging/wind.dir.burabay.daily.2014P.txt")
Sys.timezone()
library(lubridate)
data.burabay2014$date<-parse_date_time(data.burabay2014$date, order = "dmy")
is.POSIXct(data.burabay2013$date)
data.burabay2014$date<-as.POSIXct(data.burabay2014$date, tz="Asia/Dhaka")
tz(data.burabay2014$date)
View(data.burabay2014)

# Make daily averages 2014
data.burabay2014 <- timeAverage(data.burabay2014, avg.time = 'day')
head(data.burabay2014, 5)
View(data.burabay2014)

# write dataframe to file
write.table(data.burabay2014, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2014d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.burabay2014, breaks = 6)
# plot wind rose monthly
windRose(data.burabay2014, type="month", breaks = 6)

# do the same for 2015 Burabay new
# read and plot windroses in R 2015
data.burabay2015 <- read.delim("D:/R/R.code/Wind Averaging/wind.dir.burabay.daily.2015P.txt")
Sys.timezone()
library(lubridate)
data.burabay2015$date<-parse_date_time(data.burabay2015$date, order = "dmy")
is.POSIXct(data.burabay2015$date)
data.burabay2015$date<-as.POSIXct(data.burabay2015$date, tz="Asia/Dhaka")
tz(data.burabay2015$date)
View(data.burabay2015)

# Make daily averages 2015
data.burabay2015<- timeAverage(data.burabay2015, avg.time = 'day')
head(data.burabay2015, 5)
View(data.burabay2015)

# write dataframe to file
write.table(data.burabay2015, file = 'D:/R/R.code/Wind Averaging/data.davis.day.2015d.txt', sep = ',') 

# first try to plot wind rose
windRose(data.burabay2015, breaks = 6)
# plot wind rose monthly
windRose(data.burabay2015, type="month", breaks = 6)