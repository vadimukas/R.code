## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
d2m<- brick("ERA_Td_20012-2016.nc")
d2m<-d2m-273.15
d2m

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Shuchinsk<-SpatialPoints(cbind(70.25, 52.25)) 
d2m_shuchinsk<-extract(d2m,SpatialPoints(cbind(70.25, 52.75)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(d2m)
year<-as.numeric(substr(names(d2m),2,5))
month<-as.numeric(substr(names(d2m),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,d2m_shuchinsk, type="l", col='blue')

# write output in txt file
write.table(d2m_shuchinsk, file = 'd2md_shuchinks_2008-2011', sep = ',')