## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
u10<- brick("ERA wind 10 2016.nc")
u10

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Shuchinsk<-SpatialPoints(cbind(70.25, 52.75)) 
u10_shuchinsk<-extract(u10,SpatialPoints(cbind(70.25, 52.75)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(u10)
year<-as.numeric(substr(names(u10),2,5))
month<-as.numeric(substr(names(u10),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,u10_shuchinsk, type="l", col='blue')

# write output in txt file
write.table(u10_shuchinsk, file = 'u10_shuchinks', sep = ',')