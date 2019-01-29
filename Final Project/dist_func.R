my_dist_minkowski <- function(point1, point2, order){
  dist <- sum(abs(point1-point2)^order)^(1/order)
  return(dist)
}

dtw_cost_matrix <- function(ts_a, ts_b){
  # Create cost matrix
  M <- length(ts_a)
  N <- length(ts_b)
  cost <- matrix(c(Inf), nrow = M, ncol = N)
  # Initialize the first row and column
  cost[1, 1] <- my_dist_minkowski(ts_a[1], ts_b[1], 1)
  for (i in 2:M){
    cost[i, 1] <- cost[i-1, 1] + my_dist_minkowski(ts_a[i], ts_b[1], 1)
  }
  for (j in 2:N){
    cost[1, j] <- cost[1, j-1] + my_dist_minkowski(ts_a[1], ts_b[j], 1)
  }
  # Populate rest of cost matrix within window
  for (i in 2:M){
    for (j in 2:N){
      choices <- c(cost[i - 1, j - 1], cost[i, j-1], cost[i-1, j])
      cost[i, j] <- min(choices) + my_dist_minkowski(ts_a[i], ts_b[j], 1)
    }
  }
  return(cost)
}

dtw_distance <- function(ts_a, ts_b){
  # Create cost matrix
  M <- length(ts_a)
  N <- length(ts_b)
  cost <- dtw_cost_matrix(ts_a, ts_b)
  return(cost[M, N])
}

#Computes distance matrix between the training dataset and testing dataset using the DTW distance measure
dist_matrix <- function(train_list, test_list) {
  x_s <- length(train_list)
  y_s <- length(test_list)

  train_list_without_null <- list()
  not_null_list_indices <- c()
  k <- 1
  for(i in 1:x_s){
    if (is.null(train_list[[i]]) != TRUE){
      train_list_without_null[[k]] <- train_list[[i]]
      not_null_list_indices <- c(not_null_list_indices,i)
      k <- k+1
    }
  }
  
  test_list_without_null <- list()
  not_null_list_indices_test <- c()
  k <- 1
  for(i in 1:y_s){
    if (is.null(test_list[[i]]) != TRUE){
      test_list_without_null[[k]] <- test_list[[i]]
      not_null_list_indices_test <- c(not_null_list_indices_test,i)
      k <- k+1
    }
  }
  
  dm <- matrix(c(0), nrow = length(not_null_list_indices), ncol = length(not_null_list_indices_test))
  for (i in 1:length(not_null_list_indices)){
    for (j in 1:length(not_null_list_indices_test)){
      dm[i, j] <- dtw_distance(train_list_without_null[[i]]$points, test_list_without_null[[j]]$points)
    }
  }
  rownames(dm) <- not_null_list_indices
  colnames(dm) <- not_null_list_indices_test
  return(dm)
}