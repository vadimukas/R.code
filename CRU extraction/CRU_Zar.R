library(rgdal)
library(raster)
library(maptools)


setwd("C:/Users/le920588/Desktop/Map_for_paper/CRU/cru_data")
cru_pet1<-brick("cru_ts4.03.1991.2000.pet.dat.nc")
cru_pet2<-brick("cru_ts4.03.2001.2010.pet.dat.nc")



#Ala_archa
crop_extent1<- readOGR("C:/Users/le920588/Desktop/Map_for_paper/CRU/basin_shp/Ala_Archa_KG.shp")
plot(crop_extent1,
     main = "Shapefile imported into R - crop extent",
     axes = TRUE,
     border = "blue")
crs(crop_extent1)
crs(Cru_data_pet1)
Ala_archa__shp= spTransform(crop_extent1, crs(Cru_data_pet1))
Ala_Archa<-crop(Cru_data_pet1, Ala_archa__shp)
Ala_archa_text = as.data.frame(Ala_Archa, xy=TRUE)
write.table(Ala_archa_text, file = 'Ala_archa_pet_2000_2010.txt', sep = ',')
plot(Ala_Archa)

#Chirchik
crop_extent2<- readOGR("C:/Users/le920588/Desktop/Map_for_paper/CRU/basin_shp/chirchik_uz.shp")
plot(crop_extent2,
     main = "Shapefile imported into R - crop extent",
     axes = TRUE,
     border = "blue")
crs(crop_extent2)
crs(Cru_data_pet1)
Chirchik_shp=spTransform(crop_extent2, crs(Cru_data_pet1))
Chirchik<-crop(Cru_data_pet1, Chirchik_shp)
Chirchik_text = as.data.frame(Chirchik, xy=TRUE)
write.table(Chirchik_text, file = 'Chirchik_pet_2000_2010.txt', sep = ',')
plot(Chirchik)

#Kaskelen
crop_extent3<- readOGR("C:/Users/le920588/Desktop/Map_for_paper/CRU/basin_shp/kaskelen_all_kz.shp")
plot(crop_extent3,
     main = "Shapefile imported into R - crop extent",
     axes = TRUE,
     border = "blue")
crs(crop_extent3)
crs(Cru_data_pet1)
Kaskelen_shp=spTransform(crop_extent3, crs(Cru_data_pet1))
Kaskelen<-crop(Cru_data_pet1, Kaskelen_shp)
Kaskelen_text = as.data.frame(Kaskelen, xy=TRUE)
write.table(Kaskelen_text, file = 'Kaskelen_pet_2001_2010.txt', sep = ',')
plot(Kaskelen)

#Kofarnihon
crop_extent4<- readOGR("C:/Users/le920588/Desktop/Map_for_paper/CRU/basin_shp/kofarnihon_tg.shp")
plot(crop_extent4,
     main = "Shapefile imported into R - crop extent",
     axes = TRUE,
     border = "blue")
crs(crop_extent4)
crs(Cru_data_pet1)
Kofarnihon_shp=spTransform(crop_extent4, crs(Cru_data_pet1))
Kofarnihon<-crop(Cru_data_pet1, Kofarnihon_shp)
Kofarnihon_text = as.data.frame(Kofarnihon, xy=TRUE)
write.table(Kofarnihon_text, file = 'Kofarnihon_pet_2000_2010.txt', sep = ',')
plot(Kofarnihon)

setwd("C:/Users/le920588/Desktop/Map_for_paper/CRU/cru_data")
cru_pet1<-brick("cru_ts4.03.1991.2000.pet.dat.nc")
cru_pet2<-brick("cru_ts4.03.2001.2010.pet.dat.nc")

# additionally extract PET data for an additional Kaskelen point 
## Point scale climatological data can be extracted from gridded data sets.
## Therefor you need a shp.like object in r which can be constructed by means of the function SpatialPoints()
Kaskelen<-SpatialPoints(cbind(77.25, 42.25)) 
pet1<-extract(cru_pet1,SpatialPoints(cbind(77.25, 43.25)))
pet2<-extract(cru_pet2,SpatialPoints(cbind(77.25, 43.25)))

## The date information of every grid is (unfortunately) in the name as a character :(
## Using the functiond substr() and as.numeric() we can extract date.
names(pet1)
year<-as.numeric(substr(names(pet1),2,5))
month<-as.numeric(substr(names(pet1),7,8))
date<-ISOdate(year, month,1)

names(pet2)
year<-as.numeric(substr(names(pet2),2,5))
month<-as.numeric(substr(names(pet2),7,8))
date<-ISOdate(year, month,1)

## Plotting the CRU time series for CRU!
plot(date,pet1, type="l", col='red')
plot(date,pet2, type="l", col='red')

# write output in txt file
write.table(pet1, file = 'pet_kas1.txt', sep = ',')
write.table(pet2, file = 'pet_kas2.txt', sep = ',')
