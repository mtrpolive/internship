#######################################
##### SDM - Neophron percnopterus #####
#######################################


#### PRESENCE-ABSCENCE DATA ####

library(raster)
library(sp)
library(rgeos)
library(dismo)

library(rgbif)
Species <- occ_data(scientificName = "Neophron percnopterus", country="PT", limit=5000)
str(Species) # has $meta and $data # 
colnames(Species$data)
dim(Species$data)
write.csv(Species$data, "SDM.Trial.csv")

raw_data <- as.data.frame(Species$data)

# Select records w lat and long # 
LatLongSpp <- subset(Species$data, !is.na(decimalLatitude) & !is.na(decimalLongitude))
dim(LatLongSpp)

LatLongYear <- LatLongSpp[, c("decimalLatitude", "decimalLongitude", "year", "identifiedBy", "institutionID", "rightsHolder")] # identification columns to try and remove grid cell centre points # 
colnames(LatLongYear)
dim(LatLongYear)
View(LatLongYear)

# Plot the data points #
plot(LatLongYear$decimalLongitude, LatLongYear$decimalLatitude, pch=19, col = as.numeric(as.factor(LatLongYear$year)))


# Add a map #
library(maptools)
data(wrld_simpl)

extent.pt <- extent(c( -9.50,36.96, -6.19, 42.15))
plot(wrld_simpl, xlim=extent.pt[c(1,3)], ylim=extent.pt[c(2,4)], axes=TRUE, col="light yellow")

colours_for_plot <- c('red', 'blue', 'orange', 'pink', 'black', 'grey', 'yellow')

LatLongYear <- LatLongSpp[-which(LatLongSpp$institutionCode == 'SOVON'), c("decimalLatitude", "decimalLongitude", "year", "identifiedBy", "institutionID", "rightsHolder")] # identification columns to try and remove grid cell centre points # 
points(LatLongYear$decimalLongitude, LatLongYear$decimalLatitude, col=colours_for_plot[as.numeric(as.factor(raw_data$institutionCode))], cex=0.75)


# world map #
class(wrld_simpl)
help("SpatialPolygonsDataFrame-class")
wrld_simpl
crs(wrld_simpl)
head(wrld_simpl@data)
wrld_simpl@data$NAME
plot(wrld_simpl, col=as.numeric(as.factor(wrld_simpl@data$NAME)))


# Cleaning Data #

duplicates <- duplicated(LatLongYear[, 1:3]) # find duplicates#
table(duplicates)
LatLongYear2 <- LatLongYear[!duplicates,] # remove duplicates #


Neop.geo.1 <- LatLongYear2 # are coordinates plausible? #
coordinates(Neop.geo.1) <- ~decimalLongitude+decimalLatitude

crs(Neop.geo.1) <- crs(wrld_simpl)
ovr <- over(Neop.geo.1, wrld_simpl) # checking overlay #
head(ovr, 25)

i.mismatch <- which(is.na(ovr$NAME)) # do the records match map info? #

Neop.geo.2 <- Neop.geo.1[-i.mismatch,] # remove implausible records #

nrow(LatLongYear); nrow(LatLongYear2); Neop.geo.2



# New raster with PT extent #

rast <- raster(Neop.geo.2)
res(rast) <- 0.1 # set resolution to 1 degree #
rast
rast <- extend(rast, extent(rast)+1)

Neop.ptsgrid <- gridSample(Neop.geo.2, rast, n=0.1) # create grid #

rastpoly <- rasterToPolygons(rast)
plot(rastpoly, border="grey")
points(Neop.geo.2)
points(Neop.ptsgrid, cex=1, col='red', pch='x')



# Create background data #

set.seed(1)

pts.bg <- randomPoints(rast, 500) # random background points #

par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(rastpoly, border="grey")
points(Neop.geo.2)
plot(rastpoly, border="grey")
points(Neop.ptsgrid, cex=1, col='red', pch='x')
points(pts.bg, cex=1, col='blue', pch='x')





# Create pseudo-abscence data #

Neop.buff20 <- circles(Neop.geo.2, d=20000, lonlat=TRUE) # create buffer circles w a 20km radius #
Neop.poly20 <- polygons(Neop.buff20)
plot(Neop.poly20)


npts <- 1000 # random sample with 1000 points #
pseudoabs.samp <- spsample(Neop.poly20, npts, type='random', iter=25)

pseudoabs.cells <- cellFromXY(rast, pseudoabs.samp)
length(pseudoabs.cells)

pseudoabs.cells <- unique(pseudoabs.cells) # one point per grid cell #
length(pseudoabs.cells)

pseudoabs.points <- xyFromCell(rast, pseudoabs.cells)

plot(Neop.poly20)
points(pseudoabs.points, cex=0.75, pch=20, col='blue')
points(Neop.geo.2)


presence.cells <- cellFromXY(rast, Neop.geo.2) # avoiding pseudo-replicates#
length(presence.cells)
presence.cells <- unique(presence.cells)
length(presence.cells)
presence.points <- xyFromCell(rast, presence.cells)

par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(Neop.poly20)
points(pseudoabs.points, cex=0.75, pch=20, col='blue')
plot(Neop.poly20)
points(presence.points, cex=0.75, pch=20, col='red')
plot(Neop.poly20)
points(Neop.geo.2)



# generating presence-absence dataframe #

PAPoints <- as.data.frame(rbind( cbind(presence.points, rep(1, nrow(presence.points))), cbind(pseudoabs.points, rep(0, nrow(pseudoabs.points))) ))
colnames(PAPoints) <- c("lon","lat","Y")
PAPoints.sp <- PAPoints # Generating spatial point file # 
coordinates(PAPoints.sp) <- ~lon+lat




#### ENVIRONMENTAL DATA ####


# Download worldclim data #

WorldClim <- getData("worldclim", var="bio", res=10)
plot(WorldClim)
WorldClim

worldclim.pt <- crop(WorldClim, extent(rast)) # crop data to raster extent #
plot( worldclim.pt)
plot( worldclim.pt[[1]])


worldclim.pt <- projectRaster(worldclim.pt, crs(rast))
worldclim.pt <- resample(worldclim.pt, rast)
plot(worldclim.pt[[1]])
points(PAPoints$lon, PAPoints$lat, col= PAPoints$Y)
plot(wrld_simpl, add=T)




# Extracting values of predictors #
 
bio1.pts <- extract(worldclim.pt[[1]], PAPoints.sp)
worldclim.pts <- extract(worldclim.uk, PAPoints.sp)


pairs(worldclim.pts[,1:5], cex=0.1) # check correlations between bioclimatic variables #


# Presence-absence + env data in one file #

SDM.data <- data.frame(cbind(PAPoints, worldclim.pts))
apply(is.na(SDM.data), 2, which) # checking for missing data #
SDM.data <- na.omit(SDM.data) # remove rows w missing data #

write.csv(SDM.data, file = "SDM.data.csv")




#### Model Fitting ####


fit_glm <- glm(Y ~ bio1 + bio12, family='binomial', data= SDM.data) # Bio 1 - annual mean temperature; Bio 12 - annual precipitation #
summary(fit_glm)

pred <- c("bio1","bio12")


# can't obtain the axis titles on the 2 plots #
par(mfrow=c(1,2))
for (i in 1:2) {
  xz <- data.frame(sapply(colMeans(SDM.data[,pred], na.rm=T),rep,each=50))
  xz[,pred[i]] <-seq(min(SDM.data[,pred[i]],na.rm=T),max(SDM.data[,pred[i]],na.rm=T),length=50)
  xz$z <- predict(fit_glm, newdata=xz, type='response')
  plot(xz[,i],xz$z,type='l', ylim=c(0,1), cex.lab=1.5, xlab=pred[i], ylab='Occurrence probability')
  
} 






