library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
library(wordcloud)
library(ggmap)

##########################################################################################
# DATA PREPARATION
##########################################################################################

####---------------------------------Load Data Sets-----------------------------------####

listings<-read_csv("Clustering/HW1/listings.csv")            # original listings file
reviews <- read_csv("Clustering/HW1/reviews.csv")            # original reviews file
calendar <- read_csv("Clustering/HW1/calendar.csv")          # original calendar file
listing_k <- read_csv("Clustering/HW1/listing_k.csv")        # ids, lat, lon
listing_k2 <- read_csv("Clustering/HW1/listing_k2.csv")      # ids, lat, lon, distance to attractions
load(file="Clustering/HW1/map.RData")                        # map of Boston
#load(file="Clustering/HW1/map2.RData")                       # zoomed in map of Boston
attractions <- read_csv("Clustering/HW1/attractions.csv")    # list of top 10 attractions with lat and lon

####---------------Create "listings_subset" with variables to cluster on--------------####

# Coerce price into numeric type
listings<-listings %>% mutate(newprice=as.numeric(gsub('[\\$,]','',price)))

# Rename listing_k to listings_subset
listings_subset <- listing_k2

# Add price, #beds, price per bed, distance to attractions, rating, location rating, #reviews
listings_subset <- listings_subset %>% 
  mutate(price=listings$newprice, beds=listings$beds, price_bed=round(listings$newprice/beds,2)) %>%
  mutate(rating=listings$review_scores_rating, rating_loc=listings$review_scores_location, reviews_count=listings$number_of_reviews) %>%
  mutate(property_type=listings$property_type, room_type=listings$room_type, accommodates=listings$accommodates) %>%
  filter(!is.na(price_bed)) %>%               # remove null price/beds (removes 9 observations)
  filter(beds!=0) %>%                         # remove 0 beds (removes 4 observations)
  filter(reviews_count>3)                     # at least 10 reviews (removes 2236 observations)

# Write to csv file
# fwrite(listings_subset, "Clustering/HW1/listings_subset.csv")

####-----------------------------Explore Variables--------------------------------####

hist(listings_subset$rating_loc)
hist(listings_subset$rating)
hist(listings_subset$price_bed)
hist(listings_subset$accommodates)
hist(listings_subset$beds)

####--------------------------Score Reviews on Sentiment-------------------------####

# Sentiment analysis, gives score between -5 and 5
nrc_total <- get_sentiments("afinn")

# Group reviews by listing id and filter for listings with >4 reviews 
rv <- reviews %>% group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>% filter( n >= 4) %>% select(-"n")

# Break up listing into individual words, join with rv, remove all na values
new_reviews <- reviews %>%
               group_by(listing_id) %>%
               unnest_tokens(word, comments)  %>%
               right_join(rv,by="listing_id") %>% filter(!is.na(word)) %>%
               left_join(nrc_total,by="word") %>% filter(!is.na(score))

# Find the number of words scored
score <- new_reviews %>% group_by(listing_id) %>% mutate(sscore = sum(score)) %>% distinct(listing_id,sscore)
nwords <- new_reviews %>% group_by(listing_id) %>% count(listing_id) 

# Take average, otherwise longer reviews get higher scores
sentiment <- nwords %>% left_join(score,"listing_id") %>% mutate(avgsscore = sscore/n)

# All averages are positive, so define bad reviews as something below average and standardize score
sentiment$avgsscore <- scale(sentiment$avgsscore)

hist(sentiment$avgsscore)

####-------------------------Prepare data for clustering------------------------####

# Add average scores to listings_subset
combined <- listings_subset %>% left_join(sentiment,"listing_id")

# Scale variables
combined$std.lat <- combined$lat
combined$std.lon <- combined$lon
combined$std.price_bed <- combined$price_bed
combined$std.beds <- combined$beds
combined$std.rating <- combined$rating
combined$std.rating_loc <- combined$rating_loc
combined$std.avgsscore <- combined$avgsscore
combined$std.reviews_count <- combined$reviews_count
combined$std.accommodates <- combined$accommodates
combined <- combined %>%
  mutate_at(vars(4:13,26:34), funs(scale))

# CHOOSE WHAT TO CLUSTER BY INPUTTING THE VARIABLE INDEX FROM "COMBINED"
# View column names
colnames(combined)
c_all <- cbind(combined[,c(4:13,26:33)])               # lat, lon, dist to attractions, price_bed, rating, rating_loc, avgsscore, reviews_count
c_sentiment <- cbind(combined[,c(4:13,26:27,32)])      # lat, lon, avgsscore, dist to attractions
c_sentiment2 <- cbind(combined[,32])                   # avgsscore
c_rating <- cbind(combined[,c(26:28,30,31)])           # lat, lon, price_bed, rating, rating_loc
c_rating2 <- cbind(combined[,c(30)])                   # rating, rating_loc
c_attract <- cbind(combined[,c(4:13,26:28)])           # lat, lon, dist to attractions, price_bed


##############################################################################################
# CLUSTERING FUNCTIONS
##############################################################################################

####---------------------------Function for Creating Clusters-----------------------------####

# Create function for creating clusters using hclust
create_clusters <- function (clustername){
  
  # Hierarchical clustering
  clusters.c <- hclust(dist(clustername),method="complete")
  #clusters.s <- hclust(dist(c_all), method="single")
  #clusters.a <- hclust(dist(c_all), method="average")

  # Plot clusters
  plot(clusters.c)
  #plot(clusters.s)
  #plot(clusters.a)

  return(clusters.c<<-clusters.c)
  
}

####---------------------------------Function for Pruning Clusters-------------------------####

# Create function to prune dendrogram (up to 11)
prune_clusters <- function(totalclusters){
  combined$clus <- cutree(clusters.c,totalclusters)
  clu1 <- combined %>% filter(clus == 1)
  clu2 <- combined %>% filter(clus == 2)
  clu3 <- combined %>% filter(clus == 3)
  clu4 <- combined %>% filter(clus == 4)
  clu5 <- combined %>% filter(clus == 5)
  clu6 <- combined %>% filter(clus == 6)
  clu7 <- combined %>% filter(clus == 7)
  clu8 <- combined %>% filter(clus == 8)
  clu9 <- combined %>% filter(clus == 9)
  clu10 <- combined %>% filter(clus == 10)
  clu11 <- combined %>% filter(clus == 11)
  
  return(c(clu1<<-clu1,clu2<<-clu2,clu3<<-clu3,clu4<<-clu4,clu5<<-clu5,clu6<<-clu6,clu7<<-clu7,clu8<<-clu8,clu9<<-clu9,clu10<<-clu10,clu11<<-clu11))
}

####-------------------------Function for Plotting & Summarizing Clusters-------------------------####

# Create function that plots clusters and gives summary data
plot_clusters <- function (clusternumber){
  
  plot(ggmap(map, extent="device") +
  geom_point(data = clusternumber, aes(x = lon, y = lat), color = 'red', size = 2))
  #hist(clusternumber$avgsscore)
  print(paste('Avg Sent Score:',mean(clusternumber$std.avgsscore)))
  print(paste('Avg Price/Bed:',mean(clusternumber$std.price_bed)))
  print(paste('Avg Rating:',mean(clusternumber$std.rating)))
  print(paste('Avg Location Rating:',mean(clusternumber$std.rating_loc)))
  print(paste('Avg Number Reviews:',mean(clusternumber$std.reviews_count)))
  
}

####--------------------------Function for Generating Word Clouds---------------------------####

# Create function to generate word cloud based on cluster number
word_cloud <- function (clusternumber){

  words <- as.data.frame(new_reviews) %>% right_join(clusternumber,'listing_id') %>%
          select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)
  set.seed(555)
  wordcloud(words = words$word, freq = words$n, min.freq = 20,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Paired"), scale=c(1,0.7))
}

####--------------------------------RUN CLUSTERING FUNCTIONS---------------------------------####

# Run create cluster function by inputting cluster name
create_clusters(c_all)             # 8 clusters
create_clusters(c_sentiment)       # 11 clusters
create_clusters(c_sentiment2)      # 6 clusters
create_clusters(c_rating)          # 5 clusters
create_clusters(c_rating2)         # 6 clusters
create_clusters(c_attract)         # 8 clusters

# Run pruning function by inputting total number of clusters (up to 11)
prune_clusters(8)

# Run plotting function by inputting the cluster number to plot on map
plot_clusters(clu1)
plot_clusters(clu2)
plot_clusters(clu3)
plot_clusters(clu4)
plot_clusters(clu5)
plot_clusters(clu6)
plot_clusters(clu7)
plot_clusters(clu8)
plot_clusters(clu9)
plot_clusters(clu10)
plot_clusters(clu11)

# Run word cloud function by inputting cluster number
word_cloud(clu1)
word_cloud(clu2)
word_cloud(clu3)
word_cloud(clu4)
word_cloud(clu5)
word_cloud(clu6)
word_cloud(clu7)
word_cloud(clu8)


##########################################################################################
# MAPS
##########################################################################################

####--------------------------Heat Map of Listings in Boston------------------------####

ggmap(map, extent = "device") + 
  geom_density2d(data = listings_subset, aes(x = lon, y =lat), 
                 size = 0.3) +
  stat_density2d(data = listings_subset, aes(x = lon, y = lat, 
                                             fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "black", high = "red",name = "Density") + 
  scale_alpha(range = c(0,0.3), guide = FALSE) +
  ggtitle("Density of Airbnb Properties in Boston")

####-------------------------Dot Map of Price per Bed in Boston-------------------------####

circle_size <- 0.015

ggmap(map, extent = "device") + geom_point(aes(x=lon, y=lat), 
                                           data=listings_subset, col="purple", alpha=0.1,
                                           size=listings_subset$price_bed*circle_size) +
  scale_size_continuous(range=range(listings_subset$price_bed)) +
  ggtitle("Price per Bed of Airbnb Properties in Boston")

####---------------------------Plot Attractions + Clusters on Map----------------------------####

ggmap(map, extent="device") +
  geom_point(data = clu4, aes(x=lon, y=lat), color = 'red', size = 2) +
  geom_point(data=attractions, aes(x=lon, y=lat), color = 'blue', size=4)
