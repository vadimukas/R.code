#load ggmap
library(ggmap)
# set coordinates of Burabay
burabay <-c(lon=70.25, lat=52.75)
#get map at zoom level 9
map_9 <-get_map(burabay, zoom=9, scale=1,maptype="watercolor", source="stamen")
ggmap(map_9)+
  geom_point(burabay)


#tmaping Europe
library(tmap)
data("Europe")
tm_shape(Europe)+
  tm_borders()+
  tm_fill(col="area")+
  tm_compass()+
  tmap_style("cobalt")

#load countries_sp
library(sp)
library(tmap)
countries<-readRDS(file="02_countries_sp.rds")
#load and plot kz
kz <- countries[84,]
plot(kz)
#load and plot CA
ca <- countries[158,]
plot(ca)

library(rnaturalearth)
library(sp)

#world countries
sp::plot(ne_countries())
#kz
sp::plot(ne_countries(country = 'kazakhstan'))

#states, admin level1 boundaries
sp::plot(ne_states(country = 'kazakhstan')) 

#world countries
sp::plot(ne_countries())
#kz in blue color to world countries
sp::plot(ne_countries(country = 'kazakhstan'), col="blue", add=TRUE)
