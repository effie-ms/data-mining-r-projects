rm(list = ls())

# Load datasets
train <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/train.csv",header=T)
test <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/test.csv",header=T)

train_labels_participants <- as.vector(train[,562])
train_labels_activities <- as.vector(train[,563])

# Principal Component Analysis to reduce the number of dimensions
train_pca <- prcomp(train[,1:561], center=TRUE, scale = TRUE) #built-in R stats package
train_pca_rotated_coordinates <- train_pca[["x"]]
train_pca <- as.matrix(train_pca_rotated_coordinates)
train_pca_activities <- cbind(train_pca[,1:99],train_labels_activities)
train_pca_participants <- cbind(train_pca[,1:99],train_labels_participants)

test_labels_participants <- as.vector(test[,562])
test_labels_activities <- as.vector(test[,563])

test_pca <- prcomp(test[,1:561], center=TRUE, scale = TRUE) #built-in R stats package
test_pca_rotated_coordinates <- test_pca[["x"]]
test_pca <- as.matrix(test_pca_rotated_coordinates)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/my_knn.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/har_classification_helper.R")

train_labels_activities_numeric <- as.numeric(as.factor(train_labels_activities))
activities_dictionary <- unique(cbind(train_labels_activities,train_labels_activities_numeric))

train <- as.matrix(train)
test <- as.matrix(test)
train_points <- matrix(as.numeric(train[,1:561]), nrow = nrow(train), ncol = 561)
test_points <- matrix(as.numeric(test[,1:561]), nrow = nrow(test), ncol = 561)

k <- 10
classified_points <- my_knn(cbind(train_points, train_labels_activities_numeric),  test_points, k)
res <- classified_points[,ncol(classified_points)]

activities_presumed <- test_labels_activities
activities_predicted <- get_activity(activities_dictionary, res)
print(my_accuracy(activities_predicted, activities_presumed))
t<-table(test_labels_activities,activities_predicted)
print(t)

min_x <- min(test_pca[,1])-1
max_x <- max(test_pca[,1])+1
min_y <- min(test_pca[,2])-1
max_y <- max(test_pca[,2])+1

library(ggplot2)
classified_points_df <- as.data.frame(res)
ggplot(classified_points_df, aes(test_pca[,1],test_pca[,2])) + geom_point(aes(color = activities_predicted), size = 2) +
  geom_point(aes(colour = activities_presumed), size = 1)+
  scale_x_discrete("PC1", breaks = seq(min_x,max_x,(max_x-min_x)/10))+
  scale_y_discrete("PC2", breaks = seq(min_y,max_y,(max_y-min_y)/10))+
  theme_bw() + labs(title="kNN")

k_accuracies <- matrix(c(0), nrow = 2, ncol = 10)
rownames(k_accuracies) <- c("k in KNN", "Accuracy")
for(kk in 1:10){
  k <- kk
  classified_points <- my_knn(cbind(train_points, train_labels_activities_numeric),  test_points, k)
  res <- classified_points[,ncol(classified_points)]
  activities_presumed <- test_labels_activities
  activities_predicted <- get_activity(activities_dictionary, res)
  k_accuracies[1,kk] <- kk
  k_accuracies[2,kk] <- my_accuracy(activities_predicted, activities_presumed)
}
print(k_accuracies)

# DTW classification

train_labels_participants_unique <- unique(train_labels_participants)
train_labels_activities_unique <- unique(train_labels_activities)
test_labels_activities_unique <- unique(test_labels_activities)
test_labels_participants_unique <- unique(test_labels_participants)

train_ts_matrix <- cbind(train[,1], train_labels_participants, train_labels_activities)
test_ts_matrix <- cbind(test[,1], test_labels_participants, test_labels_activities)
train_ts_list_full <- get_ts_list(train_ts_matrix, train_labels_participants_unique, train_labels_activities_unique)
test_ts_list_full <- get_ts_list(test_ts_matrix, test_labels_participants_unique, test_labels_activities_unique)

train_ts_list <- list()
train_ts_labels <- c()
for(i in 1:length(train_ts_list_full)){
  train_ts_list[[i]] <- list(participant = train_ts_list_full[[i]]$participant,
                             points = mapply(train_ts_list_full[[i]]$points, FUN=as.numeric))
  train_ts_labels[i] <- train_ts_list_full[[i]]$activity
}
  
test_ts_list <- list()
test_ts_labels <- c()
for(i in 1:length(test_ts_list_full)){
  test_ts_list[[i]] <- list(participant = test_ts_list_full[[i]]$participant,
                             points = mapply(test_ts_list_full[[i]]$points, FUN=as.numeric))
  test_ts_labels[i] <- test_ts_list_full[[i]]$activity
}

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/my_knn_dtw.R")

k_accuracies <- matrix(c(0), nrow = 2, ncol = 10)
rownames(k_accuracies) <- c("k in KNN", "Accuracy")
for (kk in 1:10){
  k <- kk
  classified_ts <- my_knn_dtw(train_ts_list,test_ts_list, train_ts_labels, k)
  activities_presumed <- test_ts_labels
  activities_predicted <- classified_ts
  k_accuracies[1,kk] <- kk
  k_accuracies[2,kk] <- my_accuracy(activities_predicted, activities_presumed)
}
print(k_accuracies)

k <- 1
classified_ts <- my_knn_dtw(train_ts_list,test_ts_list, train_ts_labels, k)
activities_presumed <- test_ts_labels
activities_predicted <- classified_ts
print(my_accuracy(activities_predicted, activities_presumed))
t<-table(activities_presumed,activities_predicted)
print(t)


# TS visualization
train_activities_labels_unique <- train_labels_activities_unique
for(k in 1:length(train_activities_labels_unique)){ #for each type of activity
  activity_label <- train_activities_labels_unique[k]
  lengthes_x <- c()
  max_ys <- c()
  min_ys <- c()
  for(j1 in 1:length(train_ts_list_full)){
    if(train_ts_list_full[[j1]]$activity == activity_label){
      ts <- train_ts_list_full[[j1]]
      max_ys <- c(max_ys, as.numeric(max(ts$points)))
      min_ys <- c(min_ys, as.numeric(min(ts$points)))
      lengthes_x <- c(lengthes_x, length(ts$points))
    }
  }
  for(j2 in 1:length(test_ts_list_full)){
    if(test_ts_list_full[[j2]]$activity == activity_label){
      ts <- test_ts_list_full[[j2]]
      max_ys <- c(max_ys, as.numeric(max(ts$points)))
      min_ys <- c(min_ys, as.numeric(min(ts$points)))
      lengthes_x <- c(lengthes_x, length(ts$points))
    }
  }
  max_length_y <- as.numeric(max(max_ys))
  min_length_y <- as.numeric(min(min_ys))
  max_length_x <- max(lengthes_x)
  first <- TRUE
  ts_counter_train <- 1
  ts_counter_test <- 1
  for(j1 in 1:length(train_ts_list_full)){
    if(train_ts_list_full[[j1]]$activity == activity_label && ts_counter_train <5){
      ts <- train_ts_list_full[[j1]]
      ts_arr <- array(as.numeric(unlist(ts$points)))
      if(first == TRUE){
        plot(ts_arr,type = "o",col = 1, xlab = "index", ylab = "PC1", main = paste("Classification of activities time series - ", activity_label), xlim = c(0, max_length_x), ylim = c(min_length_y, max_length_y))
        first <- FALSE
        ts_counter_train <- ts_counter_train +1
      }
      lines(ts_arr, type = "o", col = 1)
      ts_counter_train <- ts_counter_train +1
    }
  }
  for(j2 in 1:length(test_ts_list_full)){
    if(test_ts_list_full[[j2]]$activity == activity_label && ts_counter_test <3){
      ts <- test_ts_list_full[[j2]]
      ts_arr <- array(as.numeric(unlist(ts$points)))
      lines(ts_arr, type = "o", col = 2)
      ts_counter_test <- ts_counter_test + 1
    }
  }
}
