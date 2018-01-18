## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
t2m<- brick("ERA_t2m_1979-2016.nc")
t2m<-t2m-273.15
t2m

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Shuchinsk<-SpatialPoints(cbind(70.25, 52.25)) 
t2m_shuchinsk<-extract(t2m,SpatialPoints(cbind(70.25, 52.75)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(t2m)
year<-as.numeric(substr(names(t2m),2,5))
month<-as.numeric(substr(names(t2m),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,t2m_shuchinsk, type="l", col='blue')

# write output in txt file
write.table(t2m_shuchinsk, file = 't2m_shuchinks', sep = ',')