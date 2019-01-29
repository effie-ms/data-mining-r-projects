rm(list = ls())

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/kmeans.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/my_LOF.R")

# Load datasets
train <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/train1.csv",header=T)
test <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/test1.csv",header=T)

# Principal Component Analysis to reduce the number of dimensions
train_pca <- prcomp(train[,1:561], center=TRUE, scale = TRUE) #built-in R stats package
train_activities_labels <- as.vector(train[,563])
train_participants_labels <- as.vector(train[,562])
train_pca_rotated_coordinates <- train_pca[["x"]]

# First 100 principal components explain 95% of variance in dataset
# Selecting first 100 principal components 
train_pca <- as.matrix(train_pca_rotated_coordinates)

# Clustering (kMeans)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")

kk <- 6
res_clusters <- my_kmeans(train_pca[,1:2],kk)
centers <- res_clusters[["centers"]]
clusters <- res_clusters[["clusters"]]

idx <- clusters[,ncol(clusters)] #only indices of the clusters (the last column of clusters)
all_lof_matrices <- c()
for(cl_n in 1:kk){
  #Take 1 of the found clusters
  c1 <- centers[cl_n,1:ncol(centers)-1] #coordinates of the (cl_n)th cluster's center
  cluster_1 <- train_pca[idx == cl_n,] #coordinates of the (cl_n)th cluster's data points
  
  npoints <- nrow(cluster_1)
  points <- cluster_1[,1:2]
  
  # adjacency matrix with Mahalanobis distances between points
  dist_arr <- matrix(c(0),npoints,npoints)
  for(i in 1:npoints){
    for(j in 1:npoints){
      dist <- my_dist_minkowski(points[i,],points[j,], 2)
      dist_arr[i, j] <- dist
      dist_arr[j, i] <- dist
    }
  }
  
  k <- 2 #k for kth nearest neighbour
  k_dist <- c()
  for(i in 1:npoints){
    k_dist[i] <- kNNDist(i, dist_arr, k) #find distances for each point to the kth nearest neighbour
    print(i)
  }
  
  #Results
  lofs_comp_matrix <- matrix(c(0),npoints,4)
  colnames(lofs_comp_matrix) <- c("x", "y", "dist from center", "LOF")
  for(i in 1:npoints){
    lofs_comp_matrix[i,1:2] <- points[i,1:2] #from which point (x,y)
    lofs_comp_matrix[i,3] <- my_dist_minkowski(c1,points[i,], 2) #distance from (x,y) to the center
    lofs_comp_matrix[i,4] <- LOF(i, k_dist, dist_arr) #LOF of (x,y)
  }
  
  #print(lofs_comp_matrix)
  plot(lofs_comp_matrix[,3], lofs_comp_matrix[,4], xlab="distance from center", ylab="LOF", main = paste("LOF - cluster ", cl_n))
  #abline(lm(lofs_comp_matrix[,3] ~ lofs_comp_matrix[,4])) #regression line
  lofs_comp_matrix <- cbind(c(cl_n), lofs_comp_matrix)
  all_lof_matrices <- rbind(all_lof_matrices, lofs_comp_matrix)
}
print(all_lof_matrices)

points_lof_ordered <- all_lof_matrices[order(all_lof_matrices[,5], decreasing = TRUE),]
n <- 10
n_points_with_max_lof <- points_lof_ordered[1:n,]

clusters_with_outliers <- clusters
for(i in 1:nrow(clusters_with_outliers)){
  for(j in 1:nrow(n_points_with_max_lof)){
    if(clusters_with_outliers[i,1] == n_points_with_max_lof[j,2] && clusters_with_outliers[i,2] == n_points_with_max_lof[j,3]){
      clusters_with_outliers[i,3] <- 100
    }
  }
}

min_x <- min(train_pca[,1])-1
max_x <- max(train_pca[,1])+1
min_y <- min(train_pca[,2])-1
max_y <- max(train_pca[,2])+1

clusters_and_outliers <- as.character(clusters_with_outliers[,3])
for(i in 1:length(clusters_and_outliers)){
  if(clusters_and_outliers[i] == "100"){
    clusters_and_outliers[i] <- "outlier"
  }
}

clusters_with_outliers <- as.data.frame(clusters_with_outliers)
library(ggplot2)  
ggplot(clusters_with_outliers, aes(clusters_with_outliers[,1],clusters_with_outliers[,2])) + geom_point(aes(color = clusters_and_outliers)) + 
  scale_x_continuous("PC1", breaks = seq(min_x,max_x,(max_x-min_x)/10))+
  scale_y_continuous("PC2", breaks = seq(min_y,max_y,(max_y-min_y)/10))+
  theme_bw() + labs(title="k-Means with outliers")
