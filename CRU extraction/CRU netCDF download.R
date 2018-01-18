## In order to work with gridded data in R we need additional packages.
## gdal is able to read and write various gridded data formats
## raster provides several raster-otions in r!
library(rgdal)
library(raster)
library(maptools)
library(ncdf4)

#  function brick() reads different raster-formats into the R workspace!
pet<- brick("cru_ts3.24.01.2011.2015.pet.dat.nc")
pet

## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Kokshetau<-SpatialPoints(cbind(69.25, 53.25)) 
pet_Kokshetau<-extract(pet,SpatialPoints(cbind(69.25, 53.25)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(pet)
year<-as.numeric(substr(names(pet),2,5))
month<-as.numeric(substr(names(pet),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,pet_Kokshetau, type="l", col='red')

# write output in txt file
write.table(pet_Kokshetau, file = 'pet_kokshetau', sep = ',')
