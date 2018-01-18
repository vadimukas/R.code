## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
cld<- brick("cru_ts3.24.01.2011.2015.cld.dat.nc")
cld

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Shuchinsk<-SpatialPoints(cbind(70.25, 52.75)) 
cld_shuchinsk<-extract(cld,SpatialPoints(cbind(70.25, 52.75)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(cld)
year<-as.numeric(substr(names(cld),2,5))
month<-as.numeric(substr(names(cld),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,cld_shuchinsk, type="l", col='blue')

# write output in txt file
write.table(cld_shuchinsk, file = 'cld_shuchinks', sep = ',')
