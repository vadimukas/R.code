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