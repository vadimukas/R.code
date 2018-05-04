# load package open air and lubridate
library(openair)
library(lubridate)
library(methods)

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

# first try to plot wind rose
windRose(data.davis2014, breaks = 6)
# plot wind rose monthly
windRose(data.davis2014, type="month", breaks = 6)

# Read prepared daily wind data file 2014 and format to POSIXCT
data.davis2014d <- read.delim("D:/R/R.code/Wind Averaging/wind.dir.davis.daily.2014.txt")
Sys.timezone()
data.davis2014d$date<-as.POSIXct(data.davis2014d$date,format ="%Y%m%d", tz="Asia/Dhaka")
tz(data.davis2014d$date)
is.POSIXct(data.davis2014d$date)
View(data.davis2014d)

# first plot wind rose based on daily data 2014
windRose(data.davis2014d, na="-999.0",   breaks = 6)
# plot daily wind rose monthly
windRose(data.davis2014d, type="month", breaks = 6)


