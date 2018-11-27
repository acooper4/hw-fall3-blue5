library(dplyr)
library(tidyr)
library(ggplot2)
library(neuralnet)

train <- read.csv("/Users/anniecooper/Documents/MSA_2019/Fall 3/Machine Learning/Final Project/MLProjectData_train.csv", header = TRUE)
test <- read.csv("/Users/anniecooper/Documents/MSA_2019/Fall 3/Machine Learning/Final Project/MLProjectData_test.csv", header = TRUE)

####---------------------------------- Data Exploration ------------------------------------####

# coerce original data into numeric
for (each in colnames(train)) {
  train[each] <- as.double(unlist(train[each]))
}

# visually inspect data with histogram plots
train %>% gather() %>% head()
ggplot(gather(train), aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~key, scales = 'free_x')
colnames(train)

# Check for NA's
apply(train, 2, function(x) sum(is.na(x)))

####--------------------------- Encode categorical variables and scale data --------------------------####

# Encode cat1 and cat2 as one hot vector multilabel data (not sure if order matters)
train <- cbind(train[, c(1:59,62:85)], class.ind(as.factor(train$cat1)), class.ind(as.factor(train$cat2)), train[,c(86)])

# Set column names
names(train) <- c(names(train)[c(1:83)],"cat1L1","cat1L2","cat1L3","cat1L4","cat1L5","cat2L1","cat2L2","cat2L3","cat2L4","cat2L5","cat2L6","cat2L7","cat2L8","cat2L9","cat2L10","cat2L11","cat2L12","target")

# Use range scale
maxs <- apply(train,2,max)
mins <- apply(train,2,min)
scaled <- as.data.frame(scale(train, center=mins, scale=maxs - mins))

####------------------------------- Neural Network Model Building------------------------------------- ####

# Get variable names into a list
n <- names(scaled)

# Create formula with all variables [-target]
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))

# Determine optimal number of neurons
set.seed(3472325)
cv.error <- NULL
for(j in 1:10){
  # Train-test split
  index <- sample(1:nrow(scaled),round(0.70*nrow(scaled)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  # NN fitting
  nn <- neuralnet(f,data=train.cv,hidden=j,linear.output=T)
  
  # Predicting
  pr.nn <- compute(nn,test.cv[,1:100])
  
  # Scaling back the predicted results
  pr.nn <- pr.nn$net.result*(max(train$target)-min(train$target))+min(train$target)

  # Real results
  test.cv.r <- (test.cv$target)*(max(train$target)-min(train$target))+min(train$target)
  
  # Calculating MAE test error
  cv.error[j] <- mean(abs(test.cv.r - pr.nn))
}

# Print out test error
cv.error

# Plot test error
plot(cv.error,main='MAE vs hidden neurons',xlab="Hidden neurons",ylab='Test error MAE',type='l',col='blue',lwd=2)

# Number of neurons (index) that minimizes test error
which(min(cv.error) == cv.error)

####------------------------------------ Cross Validation ------------------------------------- ####

# Cross Validation
set.seed(3472325)
cv.error <- NULL
for(j in 1:20){
  # Train-test split
  index <- sample(1:nrow(scaled),round(0.70*nrow(scaled)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  # NN fitting
  nn <- neuralnet(f,data=train.cv,hidden=2,linear.output=T)
  
  # Predicting
  pr.nn <- compute(nn,test.cv[,1:100])
  
  # Scaling back the predicted results
  pr.nn <- pr.nn$net.result*(max(train$target)-min(train$target))+min(train$target)
  
  # Real results
  test.cv.r <- (test.cv$target)*(max(train$target)-min(train$target))+min(train$target)
  
  # Calculating MAE test error
  cv.error[j] <- mean(abs(test.cv.r - pr.nn))
}

# Print out test error
mean(cv.error)

####----------------------------- Predict Target for actual test set ------------------------####

# coerce original data into numeric
for (each in colnames(test)) {
  test[each] <- as.double(unlist(test[each]))
}

# Encode cat1 and cat2 as one hot vector multilabel data (hard code 0's because cat2 in test data only goes up to 5)
test2 <- cbind(test[, c(2:60,63:86)], class.ind(as.factor(test$cat1)), class.ind(as.factor(test$cat2)), 0,0,0,0,0,0,0,0,0)

# Set column names
names(test2) <- c(names(test2)[c(1:83)],"cat1L1","cat1L2","cat1L3","cat1L4","cat1L5","cat2L1","cat2L2","cat2L3","cat2L4","cat2L5","cat2L6","cat2L7","cat2L8","cat2L9","cat2L10","cat2L11","cat2L12","target")

# Scale the values using the training data 
test.s <- as.data.frame(scale(test2, center=mins, scale=maxs - mins))

# Predictions for the test set
test_pred <- compute(nn, test.s[,1:100])

# Unscale the values
test_pred <- test_pred$net.result*(max(train$target)-min(train$target))+min(train$target)

# Add index to the final test df
test_df <- as.data.frame(seq.int(nrow(test)))

# Bind prediction to final test df
test_df <- cbind.data.frame(test_df,test_pred)

# Rename columns
names(test_df) <- c("Row", "Prediction")

# Write to csv
write.csv(test_df, "Blue5.csv", row.names=FALSE)
