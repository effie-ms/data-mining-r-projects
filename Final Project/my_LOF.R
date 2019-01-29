#Get the list of indices of minimum values in arr
get_indices_of_min <- function(arr) {
  indices_list <- c()
  min_val <- min(arr)
  for(i in 1:length(arr)){
    if(arr[i] == min_val){
      indices_list <- c(indices_list,i)
    }
  }
  return(indices_list)
}

#Get a list of indices sorted in an ascending order of distances
get_sorted_by_dist_indices <- function(arr){
  nmin_idx <- c()
  while (length(arr) > 0) {
    min_idx <- get_indices_of_min(arr)
    rest <- c()
    for(i in 1:length(arr)){
      flag <- FALSE
      for(j in 1:length(min_idx)){
        if (i == min_idx[j]){
          flag <- TRUE
        }
      }
      if(flag == FALSE){
        rest <- c(rest, arr[i])
      }
    }
    arr <- rest
    nmin_idx <- c(nmin_idx, min_idx)
  }
  return(nmin_idx)
}


#Distance from x to its k-nearest neighbour
kNNDist <- function(x_idx, dist_arr, k){
  dist2points <- dist_arr[x_idx,]
  sorted_idx <- get_sorted_by_dist_indices(dist2points)
  k_idx <- sorted_idx[k]
  return(dist2points[k_idx])
}

#Reachability distance of x with respect to y
reachDist <- function(v, x_idx, y_idx, dist_arr){
  if(dist_arr[x_idx,y_idx] > v) {
    return(dist_arr[x_idx,y_idx])
  }
  else {
    return(v)
  }
}

#Indices of elements in the set of points within kNN distance of X
getIdxInkNNDist <-function(v, distToPoints){
  l_indices <- c()
  for(i in 1:length(distToPoints)){
    if(v <= distToPoints[i]) {
      l_indices <- c(l_indices,i)
    }
  }
  return(l_indices)
}

#The average reachability distance
avgReachDist <- function(x_idx, k_dist, dist_arr){
  v <- k_dist[x_idx]
  l_arr <- getIdxInkNNDist(v, dist_arr[x_idx,])
  rsum <- 0 #sum in the numerator of the average reachability distance
  for(i in 1:length(l_arr)){
    y_idx <- l_arr[i]
    rxy <- reachDist(v, x_idx, y_idx, dist_arr)
    rsum <- rsum + rxy
  }
  return(rsum/length(l_arr))
}

#Local outlier factor
LOF <- function(x_idx, k_dist, dist_arr)
{
  arx <- avgReachDist(x_idx, k_dist, dist_arr) #the average reachability distance
  v <- k_dist[x_idx] #distance to the kth nearest neighbour
  l_arr <- getIdxInkNNDist(v, dist_arr[x_idx,]) #indices of points within the distance of kth nearest neighbour
  ar_sum <- 0 #sum in the numerator of LOF
  for(y_idx in 1:length(l_arr)){
    ary <- avgReachDist(y_idx, k_dist, dist_arr)
    ar_sum <- ar_sum + (arx / ary)
  }
  return(ar_sum / length(l_arr))
}

