rm(list = ls())

# Load datasets
train <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/train.csv",header=T)
test <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/test.csv",header=T)

# Principal Component Analysis to reduce the number of dimensions
train_pca <- prcomp(train[,1:561], center=TRUE, scale = TRUE) #built-in R stats package
train_activities_labels <- as.vector(train[,563])
train_participants_labels <- as.vector(train[,562])
train_pca_rotated_coordinates <- train_pca[["x"]]

# First 100 principal components explain 95% of variance in dataset
# Selecting first 100 principal components 
train_pca <- as.matrix(train_pca_rotated_coordinates)
train_pca_activities <- cbind(train_pca[,1:99],train_activities_labels)
train_pca_participants <- cbind(train_pca[,1:99],train_participants_labels)

# Clustering (kMeans)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/kmeans.R")

k <- 6

res_clusters <- my_kmeans(train_pca[,1:2],k)
clusters <- res_clusters[["clusters"]]
centers <- res_clusters[["centers"]]

# Scaling values for plots
min_x <- min(train_pca[,1])-1
max_x <- max(train_pca[,1])+1
min_y <- min(train_pca[,2])-1
max_y <- max(train_pca[,2])+1

# Plots clusters
clusters <- as.data.frame(clusters)
library(ggplot2)   
ggplot(clusters, aes(clusters[,1],clusters[,2])) + geom_point(aes(color = clusters[,3])) + 
  scale_x_continuous("PC1", breaks = seq(min_x,max_x,(max_x-min_x)/10))+
  scale_y_continuous("PC2", breaks = seq(min_y,max_y,(max_y-min_y)/10))+
  theme_bw() + labs(title="k-Means")

train_pca <- as.data.frame(train_pca)

ggplot(train_pca, aes(train_pca[,1],train_pca[,2])) + geom_point(aes(color = train_activities_labels)) + 
  scale_x_continuous("PC1", breaks = seq(min_x,max_x,(max_x-min_x)/10))+
  scale_y_continuous("PC2", breaks = seq(min_y,max_y,(max_y-min_y)/10))+
  theme_bw() + labs(title="Partitioning of activities")


ggplot(clusters, aes(clusters[,1],clusters[,2])) + geom_point(aes(color = train_participants_labels)) + 
  scale_x_continuous("PC1", breaks = seq(min_x,max_x,(max_x-min_x)/10))+
  scale_y_continuous("PC2", breaks = seq(min_y,max_y,(max_y-min_y)/10))+
  theme_bw() + labs(title="k-Means")


train_pca <- as.matrix(train_pca)
# Clustering evaluation
k_silhoette <- c()
for(k in 2:8) {
  res_clusters <- my_kmeans(train_pca[,1:2],k)
  clusters <- res_clusters[["clusters"]]
  silh <- silhoette_func(clusters)
  #rr <- int_dist_ratio(clusters, k)
  k_silhoette <- c(k_silhoette, silh)
}
names(k_silhoette) <- c(2:8)
print(k_silhoette)


source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/graph_clustering.R")

plot_data_all <- cbind(train_pca[,1], train_participants_labels, train_activities_labels)
train_activities_labels_unique <- unique(train_activities_labels)
for(k in 1:length(train_activities_labels_unique)){ #for each type of activity
  activity_label <- train_activities_labels_unique[k]
  print(activity_label)
  data_activity <- plot_data_all[plot_data_all[,ncol(plot_data_all)] == activity_label,1:2]
  temp <- matrix(c(0), nrow = nrow(data_activity), ncol = ncol(data_activity))
  for(i1 in 1:nrow(data_activity)){
    for(i2 in 1:ncol(data_activity)){
      temp[i1,i2] <- as.numeric(data_activity[i1,i2])
    }
  }
  data_activity <- temp
  activity_participants <- list()
  activity_participants_labels_unique <- unique(data_activity[,2])
  for(i in 1:length(activity_participants_labels_unique)){
    activity_participants[[activity_participants_labels_unique[i]]] <- list()
  }
  for(i in 1:nrow(data_activity)){ #for each record of a certain activity type
    activity_participants[[data_activity[i,ncol(data_activity)]]]$points <- c(activity_participants[[data_activity[i,ncol(data_activity)]]]$points, data_activity[i,1:(ncol(data_activity)-1)])
  }
  dtw_dist_matrix <- dist_matrix(activity_participants,activity_participants)
  nodes <- rownames(dtw_dist_matrix)
  matrix_indices <- c(1:nrow(dtw_dist_matrix))
  nodes_with_indices <- cbind(matrix_indices, nodes)
  clusters <- get_clusters_Kernighan_Lin(dtw_dist_matrix)
  print(clusters)
  cluster_nodes <- matrix(c(0), nrow = nrow(clusters), ncol = ncol(clusters))
  for(i in 1:nrow(clusters)){
    for(j in 1:ncol(clusters)){
      cluster_nodes[i, j] <- nodes_with_indices[nodes_with_indices[,1] == clusters[i,j], 2]
    }
  }
  print(cluster_nodes)
  
  lengthes_x <- c()
  for(i in 1:length(activity_participants)){
    if(is.null(activity_participants[[i]]) == FALSE){
      lengthes_x <- c(lengthes_x, length(activity_participants[[i]]$points))
    }
  }
  max_length_x <- max(lengthes_x)
  max_ys <- c()
  min_ys <- c()
  for(i in 1:length(activity_participants)){
    if(is.null(activity_participants[[i]]) == FALSE){
      max_ys <- c(max_ys, as.numeric(max(activity_participants[[i]]$points)))
      min_ys <- c(min_ys, as.numeric(min(activity_participants[[i]]$points)))
    }
  }
  max_length_y <- as.numeric(max(max_ys))
  min_length_y <- as.numeric(min(min_ys))
  
  first_ts_vect <- array((unlist(activity_participants[[as.numeric(cluster_nodes[1,1])]]$points)))
  plot(first_ts_vect,type = "o",col = 1, xlab = "index", ylab = "PC1", main = paste("Activity participants time series - ", train_activities_labels_unique[k]), xlim = c(0, max_length_x), ylim = c(min_length_y, max_length_y))
  for(j1 in 1:nrow(cluster_nodes)){
    nodes <- cluster_nodes[j1,]
    for(j2 in 1:5){#length(nodes)){
      node <- as.numeric(nodes[j2])
      ts_arr <- array(as.numeric(unlist(activity_participants[[node]]$points)))
      lines(ts_arr, type = "o", col = j1)
    }
  }
}

