#####-----my_eval_criteria.R-----#####

find_intracluster_dist <- function(p,points){
  p_size <- 0
  intra_dist_sum <- 0
  for(i1 in 1:nrow(p)){
    for(i2 in (i1+1):nrow(p)){
      if(i2 <= nrow(p)){
        intra_dist_sum <- intra_dist_sum + my_dist_func(p[i1,], p[i2,], "Minkowski2",points)
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
      inter_dist_sum <- inter_dist_sum + my_dist_func(p1[i1,], p2[i2,], "Minkowski2",points)
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
      test_points_other_clusters_with_dist[i1, ncol(test_points_other_clusters_with_dist)]<-my_dist_func(test_points_other_clusters[i1,],point,"Minkowski2",clustered_points)
    }
    test_points_other_clusters_with_dist <- test_points_other_clusters_with_dist[order(test_points_other_clusters_with_dist[,ncol(test_points_other_clusters_with_dist)], decreasing = FALSE),]
    min_intra_dist <- test_points_other_clusters_with_dist[1,ncol(test_points_other_clusters_with_dist)]
    
    test_points_same_cluster_with_dist <- cbind(test_points_same_cluster,c(0))
    for(i2 in 1:nrow(test_points_same_cluster)){
      test_points_same_cluster_with_dist[i2,ncol(test_points_same_cluster_with_dist)] <- my_dist_func(test_points_same_cluster[i2,],point,"Minkowski2",clustered_points)
    }
    avg_inter_dist <- my_sum(test_points_same_cluster_with_dist[,ncol(test_points_same_cluster_with_dist)]) / (nrow(test_points_same_cluster_with_dist)-1)
    
    max_denominator <- max(min_intra_dist, avg_inter_dist)
    
    silh_vect[i] <- (min_intra_dist - avg_inter_dist) / max_denominator
  }
  return(silh_vect)
}

#####-----End of my_eval_criteria.R-----#####