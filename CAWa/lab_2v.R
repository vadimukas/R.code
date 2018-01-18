# install packages: 
"raster, ncdf, rgdal, maptools"
library(raster)
library(ncdf4)
library(rgdal)
library(maptools)

# extract gridded data

temp<-brick("gridded_data/var_167_1979-2015.nc")
plot (temp)
temp<-temp-273
plot(temp)
temp_mean<-mean(temp)
color_scale<-colorRampPalette(c("blue","white",'red'))(100)
plot(temp_mean, main="annual mean T", col=color_scale, zlim=-30,30)

# load shape data
world_map<-readShapePoly("GIS data/TM_WORLD_BORDERS-0.2")
plot(world_map, add=T)
names(temp)
year<-as.numeric(substr(names(temp),2,5))
year
month<-as.numeric(substr(names(temp),7,8))
month

par(mfrow=c(1,2))

temp_jan<-subset(temp,which(month==1))
temp_jan_mean<-mean(temp_jan)
color_scale<-colorRampPalette(c("blue","white",'red'))(100)
plot(temp_jan_mean, main="interannual mean Jan T", col=color_scale, zlim=-30,20, ylab='T,c', xlab='coordinates')
plot(world_map, add=T)

temp_july<-subset(temp,which(month==7))
temp_july_mean<-mean(temp_july)
color_scale<-colorRampPalette(c("white","yellow",'red'))(100)
plot(temp_july_mean, main="interannual mean July T", col=color_scale, zlim=-3,40, ylab='T,c', xlab='coordinates')
plot(world_map, add=T)


# Extracting values from ERA

par(mfrow=c(1,1))
Almaty<-SpatialPoints(cbind(76.82,43.26))
plot (Almaty, add=T, col="yellow")
temp_almaty<-extract(temp, Almaty)
temp_almaty<-temp_almaty[1,]
temp_almaty
plot(temp_almaty,type="l", col='red')

temp_almaty<-data.frame(year,month,temp_almaty)
head(temp_almaty)

# Trend analysis on gridded temp analysis

plot (temp_jan)
names(temp_jan)
year<-as.numeric(substr(names(temp_jan),2,5))
year

trend_jan<-calc(temp_jan,fun=function(y){lm(y~year)$coefficient[2]})
head(trend_jan)
color_scale<-colorRampPalette(c("blue","white","red"))(100)
plot(trend_jan, main="Trend mean January T", col=color_scale, zlim=-0.2,0.09, ylab='T,c', xlab='coordinates')
plot(world_map, add=T)

library(Kendall)
par(mfrow=c(1,2))
trend_sig_jan<-calc(temp_jan,fun=function(y){MannKendall(y)$sl[1]})
color_scale<-colorRampPalette(c("white","green","red"))(100)
plot(trend_sig_jan, main="Mann-Kendall significance P January", col=color_scale, zlim=0.1,1.0, ylab='T,c', xlab='coordinates')
plot(world_map, add=T)

trend_sig_july<-calc(temp_july,fun=function(y){MannKendall(y)$sl[1]})
color_scale<-colorRampPalette(c("white","green","red"))(100)
plot(trend_sig_july, main="Mann-Kendall significance P july", col=color_scale, zlim=0.1,1.0, ylab='T,c', xlab='coordinates')
plot(world_map, add=T)
