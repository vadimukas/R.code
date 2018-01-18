## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
str<- brick("ERA_str_1979-2015.nc")
str

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Shuchinsk<-SpatialPoints(cbind(70.25, 52.75)) 
str_shuchinsk<-extract(str,SpatialPoints(cbind(70.25, 52.75)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(str)
year<-as.numeric(substr(names(str),2,5))
month<-as.numeric(substr(names(str),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,str_shuchinsk, type="l", col='red')

# write output in txt file
write.table(str_shuchinsk, file = 'str_shuchinks', sep = ',')