#####-----my_kmeans-----#####

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_distfunc.R")

choose_rand_centers <- function(points,k){
  set.seed(as.numeric(Sys.time()))
  init_centers_ind <- sample(seq_len(nrow(points)), size = k)
  init_centers <- matrix(c(0), nrow=k, ncol=ncol(points)+1)
  for(i in 1:k){
    init_centers[i,1:ncol(points)] <- points[init_centers_ind[i],1:ncol(points)]
    init_centers[i,ncol(points)+1] <- i
  }
  return(init_centers)
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

calc_dist_to_centers <- function(centers,point, points,dist_func){
  distances <- matrix(c(0),nrow=nrow(centers),ncol=2)
  for(i in 1:nrow(centers)){
    distances[i,1] <- my_dist_func(centers[i,1:ncol(centers)-1], point, dist_func, points)
    distances[i,2] <- i
  }
  return(distances)
}

calc_clusters <- function(points, init_centers, dist_func){
  for(i in 1:nrow(points)){
    point <- points[i,1:ncol(points)-1]
    dists <- calc_dist_to_centers(init_centers, point,points[,1:ncol(points)-1], dist_func)
    dists <- dists[order(dists[,1], decreasing = FALSE),]
    points[i,ncol(points)] <- dists[1,2]
  }
  return(points)
}

update_mean <- function(points){
  avg <- c()
  for(i in 1:ncol(points)-1){
    avg[i] <- mean(points[,i])
  }
  return(avg)
}

update_center <- function(points){
  new_center <- update_mean(points)
  return(new_center)
}

update_centers <- function(clusters, centers, k){
  for(i in 1:k){
    i_clustered_points <- clusters[clusters[,ncol(clusters)]==i,]
    centers[i,1:2] <- update_center(i_clustered_points)
  }
  return(centers)
}

my_kmeans <- function(points,k,dist_func){
  res_clusters <- list()
  start <- FALSE
  points <- cbind(points,c(0))
  while(start == FALSE){
    centers <- choose_rand_centers(points[,1:2], k)
    clustered_points <- calc_clusters(points, centers,dist_func)
    if(!is.null(nrow(clustered_points[clustered_points[,3] == 1,])) && !is.null(nrow(clustered_points[clustered_points[,3] == 2,]))){
      start <- TRUE
    }
  }
  changed <- FALSE
  iter_count <- 0
  while(changed == FALSE & iter_count < 100){
    centers <- update_centers(clustered_points, centers, k)
    old_clustered_points <- clustered_points
    clustered_points <- calc_clusters(old_clustered_points, centers,dist_func)
    changed <- equal_vectors(old_clustered_points[,ncol(old_clustered_points)], clustered_points[,ncol(clustered_points)]) 
    iter_count <- iter_count + 1
    if(is.null(nrow(clustered_points[clustered_points[,3] == 1,])) || is.null(nrow(clustered_points[clustered_points[,3] == 2,]))){
      clustered_points <- old_clustered_points
      break
    }
  }
  res_clusters[["centers"]] <- centers
  res_clusters[["clusters"]] <- clustered_points
  return(res_clusters)
}

#####-----End of my_kmeans-----#####