library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
library(wordcloud)
library(ggmap)
library(factoextra)
library(caret)

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

# Coerce data into correct data types
listings<-listings %>% 
  mutate(price2 = as.numeric(gsub('[\\$,]','',listings$price))) %>% 
  mutate(host_response_rate2 = as.numeric(gsub('[\\%]','',listings$host_response_rate))) %>%
  mutate(property_type2 = factor(listings$property_type,levels = c('Apartment','House','Condominium','Villa','Townhouse','Bed & Breakfast','Loft','Boat','Dorm','Other','NA'),labels = c(1,2,3,4,5,6,7,8,9,10,11))) %>%
  mutate(room_type2 = factor(listings$room_type,levels = c('Private room','Entire home/apt','Shared room'),labels = c(1,2,3)))

# Data subset
listings_subset <- listing_k2 %>% 
  mutate(price=listings$price2, beds=listings$beds, price_bed=round(listings$price2/beds,2)) %>%
  mutate(rating=listings$review_scores_rating, reviews_month=listings$reviews_per_month, reviews_count=listings$number_of_reviews) %>%
  mutate(property_type=listings$property_type2, room_type=listings$room_type2, accommodates=listings$accommodates) %>%
  mutate(host_response_rate=listings$host_response_rate2, host_listing_count=listings$host_listings_count, bathrooms=listings$bathrooms, bedrooms=listings$bedrooms) %>%
  filter(!is.na(price_bed)) %>%               # remove null price/beds (removes 9 observations)
  filter(beds!=0) %>%                         # remove 0 beds (removes 4 observations)
  filter(reviews_count>3)                     # at least 3 reviews

# Remove any nulls
listings_subset <- na.omit(listings_subset)

# Write to csv file
fwrite(listings_subset, "Clustering/HW1/listings_subset.csv")

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
# sentiment$avgsscore <- scale(sentiment$avgsscore)

hist(sentiment$avgsscore)

####-------------------------Prepare data for clustering------------------------####

# Add average scores to listings_subset
combined <- listings_subset %>% left_join(sentiment,"listing_id")

# Scale variables
colnames(combined)
combined_scale <- combined %>%
  mutate_at(vars(4:19, 22:29), funs(scale))


##############################################################################################
# HIERARCHICAL CLUSTERING FUNCTIONS
##############################################################################################

####-------------------Function for Creating Hierachical Clusters-------------------------####

# Create function for creating clusters using hclust
create_clusters <- function (clustername){
  
  # Hierarchical clustering
  clusters.c <- hclust(dist(clustername),method="complete")
  #clusters.s <- hclust(dist(c_all), method="single")
  #clusters.a <- hclust(dist(c_all), method="average")

  # Dendrogram
  plot(clusters.c)
  #plot(clusters.s)
  #plot(clusters.a)

  return(clusters.c<<-clusters.c)
  
}

####----------------------Function for Determining Optimal Cluster Number (WSS)--------------####

complete_Clust<-function(x,k){
  return(hcut(x,k, hc_method ="complete" , hc_metric="euclidian"))
}

optimal_clusters <- function(clustername){
  fviz_nbclust(as.matrix(clustername), complete_Clust, method = "wss")
}

####---------------------------------Function for Pruning Clusters-------------------------####

# Create function to prune dendrogram
prune_clusters <- function(totalclusters){
  combined_scale$clus <- cutree(clusters.c,totalclusters)
  clu <- list()
  for( i in 1:totalclusters){
    clu[[i]] <-  combined_scale %>% filter(clus == i)
  }
  return(clu<<-clu)
}

##########################################################################################
# PCA -> CLUSTERING
##########################################################################################

####----------------------FUNCTION for PCA and Optimal Clusters-----------------------####

# PCA > HIERARCHICAL
pca_hclust <- function(data){
  pc <- princomp(combined_pca)
  pc$loadings
  pc$center
  pc$scores[1,]
  
  clusters.c<-hclust(dist(pc$scores),method="complete")
  plot(clusters.c)
  
  # Optimal Cluster Number
  plot(fviz_nbclust(pc$scores, complete_Clust, method = "wss"))

  return(c(pc<<-pc))
  
}

# PCA > KMEANS
pca_kmeans <- function(data){
  pc <- princomp(combined_pca)
  pc$loadings
  pc$center
  pc$scores[1,]
  
  # Optimal Cluster Number
  plot(fviz_nbclust(pc$scores, kmeans, method = "wss",k.max=20))     # 7 clusters
  #fviz_nbclust(pc$scores, kmeans, method = "gap",k.max=20)
  plot(fviz_nbclust(pc$scores, kmeans, method = "silhouette",k.max=20))
  
  return(pc<<-pc)
  
}

####---------------------FUNCTION FOR CLUSTERING ON PC COMPONENTS-------------------####

# PCA > HIERARCHICAL
  pca_hclust_cluster <- function(df,clusternumber){
    
    # Cluster on PC Scores
    means_pc <- colMeans(pc$scores)
    pc$center
    
    # Scale PC Score
    sd_pc    <- apply(pc$scores,2,sd)
    sd_pc
    pc$sdev
    
    df$clus <- cutree(hclust(dist(pc$scores),method="complete"),clusternumber)
  
    # Interpret Clusters
    clu <- list()
    for( i in 1:clusternumber){
      clu[[i]] <-  df %>% filter(clus == i)
    }
    
    # Find the means of each cluster to "Name them"
    x <- cbind(colMeans(df))
    y <- x
    for (i in 1:clusternumber) {
      x <- cbind(x,colMeans(clu[[i]])-y)
    }
    return(c(x<<-x, clu<<-clu))
  }
  
  
# PCA > KMEANS
pca_kmeans_cluster <- function(df,clusternumber){
  
  # Cluster on PC Scores
  means_pc <- colMeans(pc$scores)
  pc$center
  
  # Scale PC scores
  sd_pc    <- apply(pc$scores,2,sd)
  sd_pc
  pc$sdev
  
  # k means with n clusters
  km <- kmeans(scale(pc$scores),clusternumber,nstart=25)
  df$clus <- km$cluster
  
  # Interpret Clusters
  clu <- list()
  for( i in 1:clusternumber){
    clu[[i]] <-  df %>% filter(clus == i)
  }
  
  # Find the means of each cluster to "Name them"
  x <- cbind(colMeans(df))
  y <- x
  for (i in 1:clusternumber) {
    x <- cbind(x,colMeans(clu[[i]])-y)
  }
  return(c(x<<-x, clu<<-clu))
}


#####################################################################################################
# VISUALIZE CLUSTERS
#####################################################################################################

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

####-------------------------Function for Plotting & Summarizing Clusters-------------------------####

# Create function that plots clusters and gives summary data
plot_clusters <- function (clusternumber){
  
  plot(ggmap(map, extent="device") +
    geom_point(data = clusternumber, aes(x = lon, y = lat), color = 'red', size = 2)) +
    geom_point(data=attractions, aes(x=lon, y=lat), color = 'blue', size=4)
  print(paste('Avg Sent Score:',mean(clusternumber$avgsscore)))
  print(paste('Avg Price/Bed:',mean(clusternumber$price_bed)))
  print(paste('Avg Rating:',mean(clusternumber$rating)))
  print(paste('Avg Reviews/Month:',mean(clusternumber$reviews_month)))
  
}

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

ggmap(map, extent = "device") + geom_point(aes(x=lon, y=lat), 
                                           data=listings_subset, col="purple", alpha=0.1,
                                           size=listings_subset$price_bed*0.02) +
  scale_size_continuous(range=range(listings_subset$price_bed)) +
  ggtitle("Price per Bed of Airbnb Properties in Boston")



########################################################################################
#---------------------------RUN FUNCTIONS STARTING HERE--------------------------------#
########################################################################################

####--------------------------RUN CLUSTERING FUNCTIONS------------------------------####

####### OPTION 1 #######

# CHOOSE WHICH VARIABLES TO CLUSTER ON
# View column names from "Combined_Scale" or "Combined" (not scaled)
colnames(combined_scale)

# Choose which variables by inputting index # from df
c_all <- cbind(combined_scale[,c(2:3,16:19,29)])

# Run create cluster function by inputting cluster name
create_clusters(c_all)             # 7 clusters

# Run optimal number of clusters using method "wss" by inputting cluster name
optimal_clusters(c_all)

# Run pruning function by inputting total number of clusters
prune_clusters(5)

# Run plotting function by inputting the cluster number "clu[[#]]" to plot on map
plot_clusters(clu[[1]])


####### OPTION 2 #######

# Use PCA to find the components of variation [everything except property, price, room type, n, sscore]
colnames(combined)
combined_pca <- combined[,-c(1,20:21,27:28)]

# Run PCA > HClust
pca_hclust(combined_pca)
pca_hclust_cluster(combined_pca,4)
x

# Run PCA > Kmeans
pca_kmeans(combined_pca)
pca_kmeans_cluster(combined_pca,7)

# View clusters
x

# Export clusters to csv
fwrite(clu[[1]], "Clustering/HW1/kmeansclu1.csv")
fwrite(clu[[2]], "Clustering/HW1/kmeansclu2.csv")
fwrite(clu[[3]], "Clustering/HW1/kmeansclu3.csv")
fwrite(clu[[4]], "Clustering/HW1/kmeansclu4.csv")
fwrite(clu[[5]], "Clustering/HW1/kmeansclu5.csv")
fwrite(clu[[6]], "Clustering/HW1/kmeansclu6.csv")
fwrite(clu[[7]], "Clustering/HW1/kmeansclu7.csv")
fwrite(rbind(clu[[1]],clu[[2]],clu[[3]],clu[[4]],clu[[5]],clu[[6]],clu[[7]]), "Clustering/HW1/kmeansclusters.csv")

# Run word cloud function by inputting cluster number "clu[[#"]]
word_cloud(clu[[1]])

# Plot all clusters together
point <- 0.02
ggmap(map, extent="device") +
  geom_point(data=attractions, aes(x=lon, y=lat), color = 'yellow', alpha=1, size=6) +
  # geom_point(data = clu[[1]], aes(x = lon, y = lat), color = 'red', alpha=0.1, size = clu[[1]]$reviews_month*point) +
  geom_point(data = clu[[2]], aes(x = lon, y = lat), color = 'purple', alpha=0.1, size = clu[[2]]$price_bed*point) +
  geom_point(data = clu[[3]], aes(x = lon, y = lat), color = 'blue', alpha=0.1, size = clu[[3]]$price_bed*point) +
  # geom_point(data = clu[[4]], aes(x = lon, y = lat), color = 'purple', alpha=0.1, size = clu[[4]]$reviews_month*point) +
  # geom_point(data = clu[[5]], aes(x = lon, y = lat), color = 'red', alpha=0.1, size = clu[[5]]$reviews_month*point) +
  # geom_point(data = clu[[6]], aes(x = lon, y = lat), color = 'yellow', alpha=0.1, size = clu[[6]]$reviews_month*point) +
  # geom_point(data = clu[[7]], aes(x = lon, y = lat), color = 'orange', alpha=0.1, size = clu[[7]]$reviews_month*point) +
  ggtitle("Airbnb Clusters by Price per Bed")
