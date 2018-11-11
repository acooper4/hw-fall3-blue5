#library(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)

####----------------------------Read in Data----------------------------####

# Import file
listings <- read_csv("Clustering/HW1/listings.csv")

# Create new dataframe listing_k
listing_k <- data.frame(listing_id=listings$id)

# Google API key
api_key <- "[insert api_key here]"
register_google(api_key)

####--------------Retrieve Lat and Lon of Listings from Google API--------------####

library(ggmap)

# Retrieve the lat and lon of each listing
strtAddress <- listings$street
lon<- matrix(0,nrow=length(strtAddress))
lat<- matrix(0,nrow=length(strtAddress))
for (ii in 1:length(strtAddress)){
   latLon <- geocode(strtAddress[ii],output="latlon", override_limit=TRUE)
   lon[ii] <- as.numeric(latLon[1])
   lat[ii] <- as.numeric(latLon[2])
}

# Add lat and lon to listing_k
listing_k <- listing_k %>% mutate(lat = lat, lon = lon)

# Write to csv file
fwrite(listing_k,"listing_k.csv")

####-----------------Generate Boston Map using Google Static Map API---------------####

map <- get_map(location = "Boston", zoom = 12)
save(map, file = "map.RData")

####---------------Retrieve Distance of Listings from Attractions-------------####

library(gmapsdistance)

# Intialize matrix with separate columns for distance to attractions
dist_attrac <- matrix(0,nrow=length(listing_k$listing_id), ncol=11)
colnames(dist_attrac)<-c("listing_id", "Isabella Stewart Gardner Museum", "Fenway Park", "MFA Boston", "Harvard Square", 
                         "Boston Harbor", "Boston Common", "Bunker Hill Monument", "Faneuil Hall Marketplace", 
                         "Arnold Arboretum", "Skywalk Observatory")

# Retrieve distance between each listing and attraction
for (i in 1:length(listing_k$listing_id)){
  for (j in 1:10){
    origin <- gsub(" ","",paste(listing_k$lat[i],'+',listing_k$lon[i]))
    destination <- gsub(" ","",paste(attractions$lat[j],'+',attractions$lon[j]))
    gdist <- gmapsdistance(origin, destination,key=api_key, mode="walking")
    dist_attrac[i,1]<-listings$id[i]
    dist_attrac[i,j+1]<-round((gdist$Distance/1609.34),2)        # convert meters to miles
  }
}

# Add distance to listing_k
dist_attrac <- as.data.frame(dist_attrac)
listing_k <- listing_k %>% left_join(dist_attrac, by="listing_id")

# Write to csv file
fwrite(listing_k,"Clustering/HW1/listing_k2.csv")
