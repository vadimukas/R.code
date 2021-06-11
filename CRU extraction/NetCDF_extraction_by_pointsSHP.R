library(rgdal)
library(raster)
library(maptools)


setwd("C:/Users/zarin/Desktop/CRU_data")
cru_pre1<-brick("cru_ts4.03.1991.2000.pre.dat.nc")

P<-readShapePoints("C:/Users/zarin/Desktop/MAP/SHP/Chirchik_points")

pre1_by_Location<-extract(cru_pre1,SpatialPoints(P))

plot(cru_pre1$X1991.01.16)
plot(P, add=T)
