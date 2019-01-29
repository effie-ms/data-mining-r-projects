source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")

choose_rand_centers <- function(points,k){
  set.seed(123)
  init_centers_ind <- sample(seq_len(nrow(points)), size = k)
  init_centers <- matrix(c(0), nrow=k, ncol=ncol(points)+1)
  for(i in 1:k){
    init_centers[i,1:ncol(points)] <- points[init_centers_ind[i],1:ncol(points)]
    init_centers[i,ncol(points)+1] <- i
  }
  return(init_centers)
}

calc_dist_to_centers <- function(centers,point, points){
  distances <- matrix(c(0),nrow=nrow(centers),ncol=2)
  for(i in 1:nrow(centers)){
    distances[i,1] <- my_dist_minkowski(centers[i,1:ncol(centers)-1], point, 2)
    distances[i,2] <- i
  }
  return(distances)
}

calc_clusters <- function(points, init_centers){
  for(i in 1:nrow(points)){
    point <- points[i,1:ncol(points)-1]
    dists <- calc_dist_to_centers(init_centers, point,points[,1:ncol(points)-1])
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
    centers[i,1:ncol(centers)-1] <- update_center(i_clustered_points)
  }
  return(centers)
}

my_kmeans <- function(points,k){
  res_clusters <- list()
  points <- cbind(points,c(0))
  centers <- choose_rand_centers(points[,1:ncol(points)-1], k)
  clustered_points <- calc_clusters(points, centers)
  changed <- FALSE
  iter_count <- 0
  while(changed == FALSE & iter_count < 100){
    centers <- update_centers(clustered_points, centers, k)
    old_clustered_points <- clustered_points
    clustered_points <- calc_clusters(old_clustered_points, centers)
    changed <- all.equal(old_clustered_points[,ncol(old_clustered_points)], clustered_points[,ncol(clustered_points)]) == TRUE 
    iter_count <- iter_count + 1
  }
  res_clusters[["centers"]] <- centers
  res_clusters[["clusters"]] <- clustered_points
  return(res_clusters)
}

find_intracluster_dist <- function(p,points){
  p_size <- 0
  intra_dist_sum <- 0
  for(i1 in 1:nrow(p)){
    for(i2 in (i1+1):nrow(p)){
      if(i2 <= nrow(p)){
        intra_dist_sum <- intra_dist_sum + my_dist_minkowski(p[i1,], p[i2,], 2)
        p_size <- p_size + 1
      }
    }
  }
  return(intra_dist_sum / p_size)
}

find_intercluster_dist <- function(p1,p2,points){
  q_size <- 0
  inter_dist_sum <- 0
  for(i1 in 1:nrow(p1)){
    for(i2 in 1:nrow(p2)){
      inter_dist_sum <- inter_dist_sum + my_dist_minkowski(p1[i1,], p2[i2,], 2)
      q_size <- q_size + 1
    }
  }
  return(inter_dist_sum / q_size)
}

int_dist_ratio <- function(clustered_points, k){
  ratios <- c()
  for(i in 1:k){
    p <- clustered_points[clustered_points[,ncol(clustered_points)]==i,1:ncol(clustered_points)-1]
    intra_dist <- find_intracluster_dist(p,clustered_points)
    p2 <- clustered_points[clustered_points[,ncol(clustered_points)]!=i,1:ncol(clustered_points)-1]
    inter_dist <- find_intercluster_dist(p,p2,clustered_points)
    ratios[i] <- intra_dist/inter_dist
    print(ratios[i])
  }
  return(ratios)
}


silhoette_func <- function(clustered_points){
  silh_vect <- c()
  for(i in 1:nrow(clustered_points)){
    point <- clustered_points[i,1:ncol(clustered_points)-1]
    point_label <- clustered_points[i,ncol(clustered_points)]
    test_points_same_cluster <- clustered_points[clustered_points[,ncol(clustered_points)]==point_label,1:ncol(clustered_points)-1]
    test_points_other_clusters <- clustered_points[clustered_points[,ncol(clustered_points)]!=point_label,1:ncol(clustered_points)-1]
    test_points_other_clusters_with_dist <- cbind(test_points_other_clusters, c(0))
    for(i1 in 1:nrow(test_points_other_clusters)){
      test_points_other_clusters_with_dist[i1, ncol(test_points_other_clusters_with_dist)]<-my_dist_minkowski(test_points_other_clusters[i1,],point,2)
    }
    test_points_other_clusters_with_dist <- test_points_other_clusters_with_dist[order(test_points_other_clusters_with_dist[,ncol(test_points_other_clusters_with_dist)], decreasing = FALSE),]
    min_intra_dist <- test_points_other_clusters_with_dist[1,ncol(test_points_other_clusters_with_dist)]
    
    test_points_same_cluster_with_dist <- cbind(test_points_same_cluster,c(0))
    for(i2 in 1:nrow(test_points_same_cluster)){
      test_points_same_cluster_with_dist[i2,ncol(test_points_same_cluster_with_dist)] <- my_dist_minkowski(test_points_same_cluster[i2,],point,2)
    }
    avg_inter_dist <- sum(test_points_same_cluster_with_dist[,ncol(test_points_same_cluster_with_dist)]) / (nrow(test_points_same_cluster_with_dist)-1)
    
    max_denominator <- max(min_intra_dist, avg_inter_dist)
    
    silh_vect[i] <- (min_intra_dist - avg_inter_dist) / max_denominator
  }
  avg_sc <- sum(silh_vect) / length(silh_vect)
  return(avg_sc)
}

