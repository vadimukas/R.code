## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
pet<- brick("cru_ts4.01.2011.2016.pet.dat.nc")
pet

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Shuchinsk<-SpatialPoints(cbind(70.25, 52.75)) 
pet_shuchinsk<-extract(pet,SpatialPoints(cbind(70.25, 52.75)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(pet)
year<-as.numeric(substr(names(pet),2,5))
month<-as.numeric(substr(names(pet),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,pet_shuchinsk, type="l", col='red')

# write output in txt file
write.table(pet_shuchinsk, file = 'pet_shuchinks.txt', sep = ',')






