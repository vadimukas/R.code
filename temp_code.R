Lqq# to write R output table to txt file
write.table(dataframename, file = 'filename', sep = ',')

## Plotting the CRU time series for CRU!
plot(date,pet_shuchinsk, type="l")

## Simple Functions for grids
sum_pet<-sum(pet)
plot(sum_pet)
plot(world_map, add=T)

world_map<-readShapePoly("GIS data/TM_WORLD_BORDERS-0.2")
plot(world_map, add=T)
plot(pet)
plot(world_map, add=T)

# coordinates Kokshetau 69.25, 53.25 
# coordinates Suchinsk 70.25, 52.75

par(mfrow = c(1, 1))
lines(TaSh$X1, xlab = "date", ylab = "Ta", col="blue")
lines(TaSh$X2, xlab = "date", ylab = "Ta", col="red")


# Is it time to ditch the Comparison of Means (T) Test?