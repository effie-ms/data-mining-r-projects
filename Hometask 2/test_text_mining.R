rm(list = ls())
library(shotGroups) #for plots

# Loading data
spam.raw <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/spam.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-16")

# Data cleaning
spam.raw <- spam.raw[1:100, 1:2]
names(spam.raw) <- c("Label","Text")
spam.raw$Label <- as.factor(spam.raw$Label)

prop.table(table(spam.raw$Label)) # the distibution of the class labels (ham vs spam)
library(caret) #to create a random train/test split and ensure the correct ham/spam class label proportions
set.seed(123) #random seed for reproducibility
indexes <- createDataPartition(spam.raw$Label, times = 1, p = 0.7, list = FALSE) # split the data into 70%/30% training/test set split
train <- spam.raw[indexes,]
test <- spam.raw[-indexes,]

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_text_processing.R")

all_tfidf <- get_tfidf(spam.raw$Text)
all_labels <- spam.raw$Label

texts_pca <- prcomp(all_tfidf, scale = TRUE) #built-in R stats package
#source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_pca.R")
#texts_pca1 <- my_pca(all_tfidf)

rotated_coordinates <- texts_pca[["x"]]
#rotated_coordinates <- texts_pca1

train_pca_tfidf <- rotated_coordinates[indexes,1:2]
labels_train <- spam.raw$Label[indexes]
test_pca_tfidf <- rotated_coordinates[-indexes,1:2]
test_labels <- spam.raw$Label[-indexes]

# Scaling values for plots
min_x <- min(test_pca_tfidf[,1])-1
max_x <- max(test_pca_tfidf[,1])+1
min_y <- min(test_pca_tfidf[,2])-1
max_y <- max(test_pca_tfidf[,2])+1

# Clustering (kMeans)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_kmeans.R")

k <- 2
res_clusters <- my_kmeans(train_pca_tfidf,k,"Minkowski2")
clusters <- res_clusters[["clusters"]]
centers <- res_clusters[["centers"]]

# Plots clusters
idx <- clusters[,ncol(clusters)]
for (i in seq(along=idx)){
  a<-switch(idx[i],"red","green")
  plot(clusters[i,1],clusters[i,2], col=a,type="p",main="k-Means",xlim=c(min_x,max_x),ylim=c(min_y,max_y), xlab="PC1",ylab="PC2")
  par(new=TRUE)
}

# Plot centroids
plot(centers[1,1],centers[1,2], col="black",type="p", pch=8, main="k-Means",xlim=c(min_x,max_x),ylim=c(min_y,max_y), xlab="PC1",ylab="PC2")
text(x=centers[1,1], y=centers[1,2]+1.5, label="Centroid1")
par(new=TRUE)
plot(centers[2,1],centers[2,2], col="blue",type="p", pch=8, main="k-Means",xlim=c(min_x,max_x),ylim=c(min_y,max_y), xlab="PC1",ylab="PC2")
text(x=centers[2,1], y=centers[2,2]+1.5, label="Centroid2")


# Classification (kNN)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 2/my_knn.R")

# Grid for the decision boundary visualization
hx <- (max_x - min_x)/100
hy <- (max_y - min_y)/100
min_max_points <- c(min_x, min_y)
for(i in 1:100){
  for(j in 1:100){
    new_point <- c(min_x + hx*i, min_y + hy*j)
    min_max_points <- rbind(min_max_points, new_point)
  }
}
all_points <- cbind(rotated_coordinates[,1:2],spam.raw$Label)
classified_points_grid <- my_knn(all_points, min_max_points, 2, "Minkowski2")

# Visualization of the grid points
yellow <- classified_points_grid[classified_points_grid[,3]==1,1:2]
blue <- classified_points_grid[classified_points_grid[,3]==2,1:2]
for(i in seq(along=yellow[,1])){
  plot(yellow[i,1],yellow[i,2],col="yellow",type="p",cex=0.5,xlim=c(min_x,max_x),ylim=c(min_y,max_y),main="kNN",xlab="PC1",ylab="PC2")
  par(new=TRUE)
}
for(i in seq(along=blue[,1])){
  plot(blue[i,1],blue[i,2],col="blue",type="p",cex=0.5,xlim=c(min_x,max_x),ylim=c(min_y,max_y),main="kNN",xlab="PC1",ylab="PC2")
  par(new=TRUE)
}

# Visualization of test set points
classified_points <- my_knn(cbind(train_pca_tfidf, labels_train), test_pca_tfidf, k, "Minkowski2")
predicted_classes<-classified_points[,ncol(classified_points)]
for(i in seq(along=test_pca_tfidf[,1])){
  a<-switch(test_labels[i], "red","green")
  b<-switch(predicted_classes[i], "red","green")
  plot(test_pca_tfidf[i,1],test_pca_tfidf[i,2],col=a,type="p",cex=2,xlim=c(min_x,max_x),ylim=c(min_y,max_y),main="kNN",xlab="PC1",ylab="PC2")
  par(new=TRUE)
  plot(test_pca_tfidf[i,1],test_pca_tfidf[i,2],col=b,type="p",pch=8,xlim=c(min_x,max_x),ylim=c(min_y,max_y),main="kNN",xlab="PC1",ylab="PC2")
  par(new=TRUE)
}

