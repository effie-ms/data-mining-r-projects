source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")

#Get the accuracy of the classification
my_accuracy <- function(predicted_labels, known_labels){
  true_values_count <- 0
  for(i in 1:length(predicted_labels)){
    if(predicted_labels[i] == known_labels[i]){
      true_values_count <- true_values_count + 1
    }
  }
  return(true_values_count / length(predicted_labels))
}

equal_vectors <- function(vector1, vector2){
  d <- length(vector1)
  equal <- TRUE
  for(i in 1:d){
    if(vector1[i] != vector2[i]){
      equal <- FALSE
    }
  }
  return(equal)
}


majority_vote <- function(points, classes_number){
  voted_labels <- matrix(c(0),classes_number,2)
  for(j in 1:classes_number){
    voted_labels[j, 1] <- j
    for(i in 1:nrow(points)){
      if(voted_labels[j, 1] == points[i, ncol(points)]){
        voted_labels[j, 2] <- voted_labels[j, 2] + 1
      }
    }
  }
  return(voted_labels)
}

find_k_points_with_min_dist <- function(points_with_distances, k){
  d <- ncol(points_with_distances)
  points_with_distances <- points_with_distances[order(points_with_distances[,d], decreasing = FALSE),]
  res_points <- points_with_distances[1:k,]
  return(res_points)
}

my_knn_classify <- function(known_data, new_point, k, classes_number){
  n <- nrow(known_data) #the number of labeled points
  d <- length(new_point) #the number of features
  points_with_distances <- matrix(c(0), nrow=n, ncol=d+1)
  for(i in 1:n){
    points_with_distances[i, 1:d] <- known_data[i, 1:d]
    points_with_distances[i, d+1] <- my_dist_minkowski(known_data[i, 1:d], new_point[1:d], 2)
  }
  k_points_with_min_dist <- find_k_points_with_min_dist(points_with_distances,k)
  nc <- ncol(k_points_with_min_dist)
  k_points_with_min_dist <- cbind(k_points_with_min_dist,c(0))
  for(i in 1:n){
    for(j in 1:nrow(k_points_with_min_dist)){
      if(equal_vectors(known_data[i,1:d], k_points_with_min_dist[j,1:d]) == TRUE){
        k_points_with_min_dist[j, nc+1] <- known_data[i,d+1]
      }
    }
  }
  voted_labels <- majority_vote(k_points_with_min_dist, classes_number)
  sorted_voted_labels <-  voted_labels[order(voted_labels[,2], decreasing = TRUE),]
  label <- sorted_voted_labels[1,1]
  return(label)
}

my_knn <- function(x_train, x_valid, k){
  classes_number <- length(unique(x_train[,ncol(x_train)]))
  res <- c()
  for(i in 1:nrow(x_valid)){
    res[i] <- my_knn_classify(x_train, x_valid[i,], k, classes_number)
    print(res[i])
  }
  x_valid <- cbind(x_valid, res)
  return(x_valid)
}
