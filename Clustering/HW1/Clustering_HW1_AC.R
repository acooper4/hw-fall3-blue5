library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
library(wordcloud)
library(ggmap)

####------------------------Load Data Sets----------------------####

listing_k<-read_csv("listing_k.csv")
reviews <- read_csv("reviews.csv")
calendar<-read_csv("calendar.csv")
load(file="map.RData")

####----------------Score Reviews and Create Clusters------------####

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
complete <- nwords %>% left_join(score,"listing_id") %>% mutate(avg = sscore/n)

# All averages are positive, so define bad reviews as something below average and standardize score
complete$avg <- scale(complete$avg)

# listing k: lat and lon of listings
combined <- complete %>% left_join(listing_k,"listing_id")
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

toC<- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toC),method="complete")
clusters.s <- hclust(dist(toC), method="single")
clusters.a <- hclust(dist(toC), method="average")

# Plot complete clusters
plot(clusters.c)

# Use 6 clusters
combined$clus <- cutree(clusters.c,6)

clu1 <- combined %>% filter(clus == 1)
clu2 <- combined %>% filter(clus == 2)
clu3 <- combined %>% filter(clus == 3)
clu4 <- combined %>% filter(clus == 4)
clu5 <- combined %>% filter(clus == 5)
clu6 <- combined %>% filter(clus == 6)


####-----------------Plot clusters on map-------------------####

# Use Google API to generate map, or load from RData
# map <- get_map(location = "Boston", zoom = 12)
# save(map, file = "map.RData")

# cluster 1, average review = -.22
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu1, aes(x = lon, y = lat), color = 'red', size = 2)
hist(clu1$avg)
mean(clu1$avg)

# cluster 2, average review = 0.92
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu2, aes(x = lon, y = lat), color = 'red', size = 2)
hist(clu2$avg)
mean(clu2$avg)

# cluster 3, average review = -2.4
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu3, aes(x = lon, y = lat), color = 'red', size = 2)
hist(clu3$avg)
mean(clu3$avg)

# cluster 4, average review = -0.05
ggmap(map, fullpage = TRUE) +
  geom_point(data = clu4, aes(x = lon, y = lat), color = 'red', size = 2)
hist(clu4$avg)
mean(clu4$avg)

# # cluster 5, average review = 0.317
# ggmap(map, fullpage = TRUE) +
#   geom_point(data = clu5, aes(x = lon, y = lat), color = 'red', size = 2)
# hist(clu5$avg)
# mean(clu5$avg)
# 
# # cluster 6, average review = -5.00
# ggmap(map, fullpage = TRUE) +
#   geom_point(data = clu6, aes(x = lon, y = lat), color = 'red', size = 2)
# hist(clu6$avg)
# mean(clu6$avg)


####----------------------Word Clouds----------------------####

words2 <- as.data.frame(new_reviews) %>% right_join(clu2,'listing_id') %>%
          select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)
 
words2 <- as.data.frame(new_reviews) %>% right_join(clu3,'listing_id')  %>%
          select(word) %>% count(word,sort=TRUE) %>% filter(n < 150)

set.seed(555)

wordcloud(words = words4$word, freq = words2$n, min.freq = 150,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(555)
wordcloud(words = words5$word, freq = words4$n, min.freq = 150,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

