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

# Check fo NA's
apply(train, 2, function(x) sum(is.na(x)))

####------------------------ Encode categorical variables and scale data ------------------------####

# Encode cat1 and cat2 as one hot vector multilabel data (not sure if order matters)
library(nnet)
train2 <- cbind(train[, c(1:59,62:85)], class.ind(as.factor(train$cat1)), class.ind(as.factor(train$cat2)), train[,c(86)])

# Set column names
names(train2) <- c(names(train2)[c(1:83)],"cat1L1","cat1L2","cat1L3","cat1L4","cat1L5","cat2L1","cat2L2","cat2L3","cat2L4","cat2L5","cat2L6","cat2L7","cat2L8","cat2L9","cat2L10","cat2L11","cat2L12","target")

# Use range scale
maxs <- apply(train2,2,max)
mins <- apply(train2,2,min)
scaled <- as.data.frame(scale(train2, center=mins, scale=maxs - mins))

####------------------------ Split training into train_t and train_v ------------------------####

# Random seed
set.seed(511)
index <- sample(1:nrow(train2),round(0.75*nrow(train2)))

# Split scaled training data into train_t and train_v
train_t.s <- scaled[index,]
train_v.s <- scaled[-index,]

####------------------------------ Neural Network -----------------------------------####

# Get variable names into a list
n <- names(train_t.s)

# Create formula with all variables [-target]
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))

# Neural network with 2 hidden layers
nn <- neuralnet(f, data=train_t.s, hidden=c(6,3), linear.output=T)
plot(nn)

# Predictions for the train_v.s set
train_v_pred <- compute(nn, train_v.s[,1:85])        # non encoded cat vars
train_v_pred <- compute(nn, train_v.s[,1:100])       # encoded cat vars

# Unscale the values
train_v_pred <- train_v_pred$net.result*(max(train$target)-min(train$target))+min(train$target)
train_v_actual <- train_v.s$target*(max(train$target)-min(train$target))+min(train$target)

# Calculate MSE for train_v.s set
MSE_nn <- sum((train_v_actual - train_v_pred)^2)/nrow(train_v.s)
MSE_nn

# Calculate MAE for train_v.s set
MAE <- mean(abs((train_v_actual - train_v_pred)))
MAE

# Calculate MAPE for train_v.s set
MAPE <- mean(abs((train_v_actual - train_v_pred))/abs(train_v_actual))
MAPE

####-------------------- Predict Target for actual test set ----------------####

# coerce original data into numeric
for (each in colnames(test)) {
  test[each] <- as.double(unlist(test[each]))
}

# Encode cat1 and cat2 as one hot vector multilabel data (hard code 0's because cat2 in test data only goes up to 5)
library(nnet)
test2 <- cbind(test[, c(2:60,63:86)], class.ind(as.factor(test$cat1)), class.ind(as.factor(test$cat2)), 0,0,0,0,0,0,0,0,0)

# Set column names
names(test2) <- c(names(test2)[c(1:83)],"cat1L1","cat1L2","cat1L3","cat1L4","cat1L5","cat2L1","cat2L2","cat2L3","cat2L4","cat2L5","cat2L6","cat2L7","cat2L8","cat2L9","cat2L10","cat2L11","cat2L12","target")

# Scale the values using the training data 
test.s <- as.data.frame(scale(test2, center=mins, scale=maxs - mins))

# Predictions for the test set
test_pred <- compute(nn, test.s[,1:100])

# Unscale the values
test_pred <- test_pred$net.result*(max(train$target)-min(train$target))+min(train$target)
