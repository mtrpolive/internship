library(rgbif)
install.packages("scrubr")
library(scrubr)
install.packages("maps")
library(maps)

# retrieve occurrence data for black-headed gull in Spain #
myspecies <- c("Chroicocephalus ridibundus")
gbif_data <- occ_data(scientificName = myspecies, hasCoordinate = TRUE, limit = 100000, country= "ES")  
gbif_data # if # of records retrieved < # records found --- increase limit#
gbif_citation(gbif_data)

names(gbif_data)
names(gbif_data$meta)
names(gbif_data$data)


# obtain columns necessary for the mapping #
myspecies_coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references", "country")]
head(myspecies_coords)

map("world", xlim = range(myspecies_coords$decimalLongitude), ylim = range(myspecies_coords$decimalLatitude))  
points(myspecies_coords[ , c("decimalLongitude", "decimalLatitude")], pch = ".", col="red")


# create data frame to plot #occurrence per year #
library(ggplot2)
data.frame(gbif_data)
ggplot(data=gbif_data, aes(x=year, y=occurrence)) + geom_bar(stat="identity")
