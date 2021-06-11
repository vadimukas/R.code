library(rgdal)
library(raster)
library(maptools)


setwd("D:/R/R.code/CRU extraction")

# extracting PET data for soil points 
cru_pet1<-brick("cru_ts4.03.2011.2018.pet.dat.nc")

P<-readShapePoints("D:/R/R.code/CRU extraction/soil_points")
pet1<-extract(cru_pet1,SpatialPoints(P), varname="pet") 
# write output in txt file
write.table(pet1, file = 'pet_2011_2018.txt', sep = ',')
# convert to monthly sums and save
pet_tot1<-round(30*pet1)

write.table(pet_tot1, file = 'pet_2011_2018_s.txt', sep = ',')

cru_pet2<-brick("cru_ts4.03.2001.2010.pet.dat.nc")

pet2<-extract(cru_pet2,SpatialPoints(P)) 
# write output in txt file
write.table(pet2, file = 'pet_2001_2010.txt', sep = ',')
# convert to monthly sums and save
pet_tot2<-round(30*pet2)
write.table(pet_tot2, file = 'pet_2001_2010_s.txt', sep = ',')

cru_pet3<-brick("cru_ts4.03.1991.2000.pet.dat.nc")
pet3<-extract(cru_pet3,SpatialPoints(P)) 
# write output in txt file
write.table(pet3, file = 'pet_1991_2000.txt', sep = ',')
# convert to monthly sums and save
pet_tot3<-round(30*pet3)
write.table(pet_tot3, file = 'pet_1991_2000_s.txt', sep = ',')

cru_pet4<-brick("cru_ts4.03.1981.1990.pet.dat.nc")
pet4<-extract(cru_pet4,SpatialPoints(P)) 
# write output in txt file
write.table(pet4, file = 'pet_1981_1990.txt', sep = ',')

# convert to monthly sums and save
pet_tot4<-round(30*pet4)
write.table(pet_tot4, file = 'pet_1981_1990_s.txt', sep = ',')


# after crunching the data make a plot 
# import the data

library(readr)
ASN_1981_2015_CRU <- read_csv("CRU extraction/ASN_1981_2015_CRU.csv", 
                              col_types = cols(N = col_integer(), Precipitation = col_integer(), 
                                               PET = col_integer()))
View(ASN_1981_2015_CRU)

library(tidyverse)

ggplot() +
  geom_col(data= ASN_1981_2015_CRU, aes(x = N, y = Precipitation, size = 0.7, colour = "blue"), show.legend = FALSE) +
  geom_point(data= ASN_1981_2015_CRU, aes(x = N, y = PET, size = 0.1, colour = "green"), show.legend = FALSE)+
   theme_test()



