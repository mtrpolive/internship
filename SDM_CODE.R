##################
###### SDM #######
##################

# 1. Importing Data #


# 1.1 Importing gbif data #

library(raster)
library(sp)
library(rgeos)
library(dismo)

library(rgbif)
SppList <- c("Aegypius monachus", "Aquila adalberti", "Gypaetus barbatus", "Neophron percnopterus", "Haematopus ostralegus", "Limosa limosa", "Numenius arquata ", "Vanellus vanellus ", "Streptopelia turtur ",
             "Alectoris rufa ", "Otis tarda ", "Tetrax tetrax ", "Anthus pratensis ", "Chersophilus duponti", "Lanius meridionalis", "Pyrrhula pyrrhula", "Saxicola rubetra", "Sylvia undata", "Turdus iliacus", "Geronticus eremita")

Species_PT <- occ_data(scientificName = SppList, country="PT", limit=5000)
str(Species_PT) # has $per spp with $met and $data # 
colnames(Species_PT$data)
dim(Species_PT$data)

 
Species_PT_LatLong <- subset(Species_PT$data, !is.na(decimalLatitude) & !is.na(decimalLongitude)) # Select records w lat and long #
dim(Species_PT_LatLong)

Species_PT_LatLongYear <- Species_PT_LatLong[, c("acceptedScientificName","decimalLatitude", "decimalLongitude", "year")]
colnames(Species_PT_LatLongYear)
dim(Species_PT_LatLongYear)




Species_ES <- occ_data(scientificName = SppList, country="ES", limit=5000)
str(Species_ES) # has $meta and $data # 
colnames(Species_ES$data)
dim(Species_ES$data)

Species_ES_LatLong <- subset(Species_ES$data, !is.na(decimalLatitude) & !is.na(decimalLongitude)) # Select records w lat and long #
dim(Species_ES_LatLong)

Species_ES_LatLongYear <- Species_ES_LatLong[, c("acceptedScientificName", "decimalLatitude", "decimalLongitude", "year")]
colnames(Species_ES_LatLongYear)
dim(Species_ES_LatLongYear)



TotalSpp_PT <- data.frame(Species_PT_LatLongYear)
TotalSpp_ES <- data.frame(Species_ES_LatLongYear)

TotalSpp_Iberian <- rbind(TotalSpp_PT, TotalSpp_ES)
dim(TotalSpp_Iberian)


write.csv(TotalSpp_Iberian, "TotalSpp_Iberian.csv")



# 1.2 Importing eBird data #

eBird_ES_PT_2 <- read.delim("C:/Users/mtrpo/OneDrive/Desktop/University/Internship/eBird_ES_PT_2.txt")
str(eBird_ES_PT_2)
colnames(eBird_ES_PT_2)
dim(eBird_ES_PT_2)



# 1.3 Merging gbif with eBird#


# 2. Cleaning data #










