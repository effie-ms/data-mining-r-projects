# clear everything
rm(list=ls())

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_kmeans.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_LOF.R")

# load the data
load(file="D:/Effie/TTU/Data Mining and Network Analysis/Practices/kNN_data1.RData")

#split the data in proportion 70/30
sample_size <- floor(0.3 * nrow(x))

set.seed(123)
train_ind <- sample(seq_len(nrow(x)), size = sample_size)

train_set <- x[train_ind, ]
train <- train_set[,1:2]

#Get clusters with k-means algorithm
k <- 3
res_clusters <- my_kmeans(train,k,"Mahalanobis")

clusters <- res_clusters[["clusters"]]
centers <- res_clusters[["centers"]]

idx <- clusters[,ncol(clusters)] #only indices of the clusters (the last column of clusters)

#Take 1 of the found clusters
cl_n <- 2 #the number of a cluster
c1 <- centers[cl_n,1:ncol(centers)-1] #coordinates of the (cl_n)th cluster's center
cluster_1 <- train[idx == cl_n,] #coordinates of the (cl_n)th cluster's data points

npoints <- nrow(cluster_1)
points <- cluster_1[,1:2]

# adjacency matrix with Mahalanobis distances between points
dist_arr <- matrix(c(0),npoints,npoints)
for(i in 1:npoints){
  for(j in 1:npoints){
    dist <- my_dist_func(points[i,],points[j,], "Mahalanobis", points)
    dist_arr[i, j] <- dist
    dist_arr[j, i] <- dist
  }
}

k <- 2 #k for kth nearest neighbour
k_dist <- c()
for(i in 1:npoints){
  k_dist[i] <- kNNDist(i, dist_arr, k) #find distances for each point to the kth nearest neighbour
}

#Results
lofs_comp_matrix <- matrix(c(0),npoints,4)
colnames(lofs_comp_matrix) <- c("x", "y", "dist from center", "LOF")
for(i in 1:npoints){
  lofs_comp_matrix[i,1:2] <- points[i,1:2] #from which point (x,y)
  lofs_comp_matrix[i,3] <- my_dist_func(c1,points[i,], "Mahalanobis", points) #distance from (x,y) to the center
  lofs_comp_matrix[i,4] <- LOF(i, k_dist, dist_arr) #LOF of (x,y)
}

#print(lofs_comp_matrix)
plot(lofs_comp_matrix[,3], lofs_comp_matrix[,4], xlab="distance from center", ylab="LOF")
#abline(lm(lofs_comp_matrix[,3] ~ lofs_comp_matrix[,4])) #regression line

