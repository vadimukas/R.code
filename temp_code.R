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
setwd("D:/R/R.code")
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
fastPOSIXct("2003-02-27","2003-02-28")

# farst_strtime() vs lubridate
library(lubridate)
parse_date_time(x, order = "ymd")
parse_date_time(local_WQm$Date, order = "dmy")

library(fasttime)
fast_strptime(x, format = "%Y-%m-%d")
fast_strptime(local_WQm$Date, format = "%d-%m-%Y")

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

#     ******************
# Explore Your Dataset in R
# https://www.littlemissdata.com/blog/simple-eda 
#Load the readr library to bring in the dataset
library(readr)

#Download the data set
df= read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)
head(df, 10)
dim(df)

#Displays the type and a preview of all columns as a row so that it's very easy to take in.

library(dplyr)
glimpse(df)
summary(df)

#Next we run the skim function from the skimr package
install.packages("skimr")
library(skimr)
skim(df)

# install and use DataExplorer package
install.packages("DataExplorer", dependencies = TRUE)
library(DataExplorer)
DataExplorer::create_report(df)

# Zooming in on maps with sf and ggplot2
# https://www.r-bloggers.com/zooming-in-on-maps-with-sf-and-ggplot2/ 

library(ggplot2)
library(sf)
library(rnaturalearth)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
# have a look at these two columns only
head(worldmap[c('name', 'continent')])

#plot worldmap
ggplot() + geom_sf(data = worldmap) + theme_bw()

# plot Kazakhstan
kazakhstan <- worldmap[worldmap$name == 'Kazakhstan',]
ggplot() + geom_sf(data = kazakhstan) + theme_bw()

europe <- worldmap[worldmap$continent == 'Europe',]
ggplot() + geom_sf(data = europe) + theme_bw()

# add-ins to make life easier
#https://www.r-bloggers.com/rstudio-addins-or-how-to-make-your-coding-life-easier/
#1 Esquisse add-in to construct ggplot plots easy
install.packages("esquisse") 

library(ggplot2)

ggplot(ICP_samples_2019) +
 aes(x = month, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#6dcd59") +
 theme_classic()

#2 questionr - you can easily reorder and recode factor variables 
install.packages("questionr")

#3 Remedy - to produce R markdown
install.packages("remedy") 

#4 Styler - to tidy up code before publishing or sharing
install.packages("styler")

# Displaying time series, spatial and space-time data with R
# https://www.r-bloggers.com/displaying-time-series-with-r/ 

library(lattice)
library(ggplot2)
# latticeExtra must be loaded after ggplot2 to prevent masking of `layer`
library(latticeExtra)
library(RColorBrewer)
# lattice and latticeExtra configuration
myTheme <- custom.theme.2(
  pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')),
  symbol=brewer.pal(n=8, name="Dark2"))
myTheme$strip.background$col = myTheme$strip.shingle$col =
  myTheme$strip.border$col = 'transparent'

myArgs <- list(
  as.table=TRUE, between=list(x=0.5, y=0.2),
  xscale.components = function(...)
    modifyList(xscale.components.default(...), list(top=FALSE)),
  yscale.components = function(...)
    modifyList(yscale.components.default(...), list(right=FALSE)))

lattice.options(default.theme=myTheme, default.args=modifyList(
  lattice.options()$default.args, myArgs))

library(zoo)

setwd ('D:/R/R.code')
# load data
load('aranjuez.RData')
load('navarra.RData')
load('CO2.RData')

# Time graph of variables with different scales
## The layout argument arranges panels in rows
xyplot(aranjuez, layout = c(1, ncol(aranjuez)))

## facet_free allows each panel to have its own range
autoplot(aranjuez) + facet_free ()

#Time series of variables with the same scale 

avRad <- zoo(rowMeans(navarra, na.rm = 1), index(navarra))
pNavarra <- xyplot(navarra - avRad,
                   superpose = TRUE, auto.key = FALSE,
                   lwd = 0.5, alpha = 0.3, col = 'midnightblue') 
pNavarra 

horizonplot(navarra - avRad,
            layout = c(1, ncol(navarra)),
            origin = 0, ## Deviations in each panel are calculated
            ## from this value
            colorkey = TRUE,
            col.regions = brewer.pal(6, "RdBu")) 

Ta <- aranjuez$TempAvg
timeIndex <- index(aranjuez)
longTa <- ave(Ta, format(timeIndex, '%j'))
diffTa <- (Ta - longTa) 

years <- unique(format(timeIndex, '%Y'))

horizonplot(diffTa, cut = list(n = 8, overlap = 0),
            colorkey = TRUE, layout = c(1, 8),
            scales = list(draw = FALSE, y = list(relation = 'same')),
            origin = 0, strip.left = FALSE) +
  layer(grid.text(years[panel.number()], x  =  0, y  =  0.1, 
                  gp = gpar(cex = 0.8),
                  just = "left")) 

year <- function(x)as.numeric(format(x, '%Y'))
day <- function(x)as.numeric(format(x, '%d'))
month <- function(x)as.numeric(format(x, '%m')) 
myTheme <- modifyList(custom.theme(region = brewer.pal(9, 'RdBu')),
                      list(
                        strip.background = list(col = 'gray'),
                        panel.background = list(col = 'gray')))

maxZ <- max(abs(diffTa))

levelplot(diffTa ~ day(timeIndex) * year(timeIndex) | factor(month(timeIndex)),
          at = pretty(c(-maxZ, maxZ), n = 8),
          colorkey = list(height = 0.3),
          layout = c(1, 12), strip = FALSE, strip.left = TRUE,
          xlab = 'Day', ylab = 'Month', 
          par.settings = myTheme)

df <- data.frame(Vals = diffTa,
                 Day = day(timeIndex),
                 Year = year(timeIndex),
                 Month = month(timeIndex)) 

library(scales) 
## The packages scales is needed for the pretty_breaks function.

ggplot(data = df,
       aes(fill = Vals,
           x = Day,
           y = Year)) +
  facet_wrap(~ Month, ncol = 1, strip.position = 'left') +
  scale_y_continuous(breaks = pretty_breaks()) + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  geom_raster() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Interactive graphics
# Dygraphs
install.packages("dygraphs")
library(dygraphs)


dyTemp <- dygraph(aranjuez[, c("TempMin", "TempAvg", "TempMax")],
                  main = "Temperature in Aranjuez",
                  ylab = "ºC")
widgetframe::frameWidget(dyTemp)



dygraph(aranjuez[, c("TempMin", "TempAvg", "TempMax")],
        main = "Temperature in Aranjuez",
        ylab = "ºC") %>%
  dySeries(c("TempMin", "TempAvg", "TempMax"),
           label = "Temperature") %>%
  widgetframe::frameWidget()

# use highcharter package
# http://jkunst.com/highcharter/ 
# https://www.datacamp.com/community/tutorials/data-visualization-highcharter-r 
# https://www.highcharts.com/demo 
# https://www.htmlwidgets.org/showcase_highcharts.html


install.packages ("highcharter")
library (highcharter)
library(xts)

aranjuezXTS <- as.xts(aranjuez)

highchart() %>%
  hc_add_series(name = 'TempMax',
                aranjuezXTS[, "TempMax"]) %>%
  hc_add_series(name = 'TempMin',
                aranjuezXTS[, "TempMin"]) %>%
  hc_add_series(name = 'TempAvg',
                aranjuezXTS[, "TempAvg"]) %>%
  widgetframe::frameWidget()

# next is plotly
library(plotly)

aranjuezDF <- fortify(aranjuez[,
                               c("TempMax",
                                 "TempAvg",
                                 "TempMin")],
                      melt = TRUE)
summary(aranjuezDF) 

plot_ly(aranjuezDF) %>%
  add_lines(x = ~ Index,
            y = ~ Value,
            color = ~ Series) %>%
  widgetframe::frameWidget()

# scatter plot matrix 
aranjuezDF <- as.data.frame(aranjuez)
aranjuezDF$Month <- format(index(aranjuez), '%m') 

## Red-Blue palette with black added (12 colors)
colors <- c(brewer.pal(n = 11, 'RdBu'), '#000000')

## Rearrange according to months (darkest for summer)
colors <- colors[c(6:1, 12:7)] 
splom(~ aranjuezDF[1:10], ## Do not include "Month"
      groups = aranjuezDF$Month,
      auto.key = list(space = 'right', 
                      title = 'Month', cex.title = 1),
      pscale = 0, varname.cex = 0.7, xlab = '',
      par.settings = custom.theme(symbol = colors,
                                  pch = 19),
      cex = 0.3, alpha = 0.1)

library(GGally)

ggpairs(aranjuezDF,
        columns = 1:10, ## Do not include "Month"
        upper = list(continuous = "points"),
        mapping = aes(colour = Month, alpha = 0.1)) 

# Scatterplot with time as a conditioning variable
library(reshape2)

aranjuezRshp <- melt(aranjuezDF,
                     measure.vars = c('TempMax',
                                      'TempAvg',
                                      'TempMin'),
                     variable.name = 'Statistic',
                     value.name = 'Temperature')

ggplot(data = aranjuezRshp, aes(Radiation, Temperature)) +
  facet_grid(Statistic ~ Month) +
  geom_point(col = 'skyblue4', pch = 19, cex = 0.5, alpha = 0.3) +
  geom_rug() +
  stat_smooth(se = FALSE, method = 'loess',
              col = 'indianred1', lwd = 1.2) +
  theme_bw()

library(latticeExtra)

useOuterStrips(
  xyplot(Temperature ~ Radiation | Month * Statistic,
         data = aranjuezRshp,
         between = list(x = 0),
         col = 'skyblue4', pch = 19,
         cex = 0.5, alpha = 0.3)) +
  layer({
    panel.rug(..., col.line = 'indianred1',
              end = 0.05, alpha = 0.6)
    panel.loess(..., col = 'indianred1',
                lwd = 1.5, alpha = 1)
  })

# Time as a complementary variable
# Polylines


## lattice version
xyplot(GNI.capita  ~ CO2.capita, data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       groups = Country.Name, type = 'b')

nCountries <- nlevels(CO2data$Country.Name)
pal <- brewer.pal(n = 5, 'Set1')
pal <- rep(pal, length = nCountries) 

## Rank of average values of CO2 per capita
CO2mean <- aggregate(CO2.capita ~ Country.Name,
                     data = CO2data, FUN = mean)
palOrdered <- pal[rank(CO2mean$CO2.capita)]  

## simpleTheme encapsulates the palette in a new theme for xyplot
myTheme <- simpleTheme(pch = 19, cex = 0.6, col = palOrdered) 

## lattice version
pCO2.capita <- xyplot(GNI.capita  ~ CO2.capita,
                      data = CO2data,
                      xlab = "Carbon dioxide emissions (metric tons per capita)",
                      ylab = "GNI per capita, PPP (current international $)",
                      groups = Country.Name,
                      par.settings = myTheme,
                      type = 'b')

## lattice version
pCO2.capita <- pCO2.capita +
  glayer_(panel.text(...,
                     labels = CO2data$Year[subscripts],
                     pos = 2, cex = 0.5, col = 'gray'))
pCO2.capita

install.packages('directlabels')
library(directlabels)

## lattice version
direct.label(pCO2.capita,
             method = 'extreme.grid')

## ggplot2 version
gCO2.capita <- ggplot(data = CO2data,
                      aes(x = CO2.capita,
                          y = GNI.capita,
                          color = Country.Name)) +
  geom_point() + geom_path() +
  scale_color_manual(values = palOrdered, guide = FALSE) +
  xlab('CO2 emissions (metric tons per capita)') +
  ylab('GNI per capita, PPP (current international $)') +
  theme_bw() 

## ggplot2 version
direct.label(gCO2.capita, method = 'extreme.grid')

# Interactive graphics: animation

p <- plot_ly(CO2data,
             x = ~CO2.capita,
             y = ~GNI.capita,
             sizes = c(10, 100),
             marker = list(opacity = 0.7,
                           sizemode = 'diameter')) 

p <- add_markers(p,
                 size = ~CO2.PPP,
                 color = ~Country.Name,
                 text = ~Country.Name, hoverinfo = "text",
                 ids = ~Country.Name,
                 frame = ~Year,
                 showlegend = FALSE) 

p <- animation_opts(p,
                    frame = 1000,
                    transition = 800,
                    redraw = FALSE)

p <- animation_slider(p,
                      currentvalue = list(prefix = "Year "))

widgetframe::frameWidget(p)

# https://statsandr.com/blog/rstudio-addins-or-how-to-make-your-coding-life-easier/ 
# Reordering factors with # {questionr}

library(questionr)
# https://statsandr.com/blog/rstudio-addins-or-how-to-make-your-coding-life-easier/#reordering-factors
  

# The Tidyverse Cookbook 

# https://rstudio-education.github.io/tidyverse-cookbook/ 

# some plots from Datacamp course on ggplot
#Introduction to Data Visualization with ggplot2 

library(tidyverse)
library(ggthemes)
ggplot(mtcars, aes(wt, mpg, color = cyl)) +
  # Set the shape and size of the points
  geom_point(shape = 1, size = 4)+
  theme_classic()

ggplot(mtcars, aes(wt, mpg, fill = cyl)) +
  # Change point shape; set alpha
  geom_point(shape = 21, size = 4, alpha = 0.6)+
  theme_light()

# Base layer
plt_mpg_vs_wt <- ggplot(mtcars, aes(wt, mpg))

# Use text layer and map fcyl to label
plt_mpg_vs_wt + geom_text(aes(label = cyl))

# making a summary stats and plotting it

iris %>%
  group_by(Species) %>%
  summarise_all(mean) -> iris.summary

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  # Inherits both data and aes from ggplot()
  geom_point(shape = 1) +
  # Different data, but inherited aes
  geom_point(data = iris.summary, shape = 19, size = 5)+
  theme_test()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  # Inherits both data and aes from ggplot()
  geom_point(shape = 1) +
  # Different data, but inherited aes
  geom_point(data = iris.summary, shape = 19, size = 5)+
  theme_wsj()

# more plots from Datacamp course on ggplot

library (gapminder)
head(gapminder)


# Set the color scale
library(RColorBrewer)
palette <- brewer.pal(5, "RdYlBu")[-(2:4)]

# Add a title and caption
top10 <- gapminder %>% top_n(10,  lifeExp)
bottom10 <- gapminder %>% top_n(-10,  lifeExp)
top_bottom10 <- gapminder %>% arrange() %>% top_frac(0.1, wt= lifeExp)
ggplot(top10, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2) +
  geom_text(aes(label = round(lifeExp,1)), color = "white", size = 1.5) +
  scale_x_continuous("", expand = c(0,0), limits = c(30,90), position = "top") +
  scale_color_gradientn(colors = palette) +
  labs(title = "Highest life expectancies", caption ="Source: gapminder" )+
  theme_test()

# formating R code with formatR 
library(formatR)
# https://yihui.org/formatr/


library(ggplot2)
library(gridExtra)
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  geom_smooth(method="loess") +
  facet_wrap(~ Species)
