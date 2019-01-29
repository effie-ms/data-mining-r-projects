source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")

get_mode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

#Predict the class labels for the provided data
predict <- function(train_list, test_list, train_labels, k) {
  dm <- dist_matrix(train_list, test_list)
  test_labels_predicted <- c()
  for(i in 1:length(test_list)){
    dist_vector <- dm[,i]
    dist_vector_with_indices <- cbind(c(1:length(dist_vector)), dist_vector)
    # Identify the k nearest neighbors
    dist_vector_with_indices <- dist_vector_with_indices[order(dist_vector_with_indices[,2], decreasing = FALSE),]
    knn_ts_indices <- dist_vector_with_indices[1:k,1]
    # Identify k nearest labels
    knn_labels <- train_labels[knn_ts_indices]
    # Mode label
    mode_label <- get_mode(knn_labels)[1]
    test_labels_predicted[i] <- mode_label
  }
  return(test_labels_predicted)
}

#kNN classifier using dynamic time warping as the distance measure between pairs of time series arrays
my_knn_dtw <- function(train_ts_list, test_ts_list, train_labels, k){
  test_labels <- predict(train_ts_list, test_ts_list, train_labels, k)
  return(test_labels)
}