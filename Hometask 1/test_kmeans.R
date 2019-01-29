rm(list=ls())

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_kmeans.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_eval_criteria.R")

library(shotGroups)

load(file="D:/Effie/TTU/Data Mining and Network Analysis/Practices/kNN_data1.RData")
sample_size <- floor(0.2 * nrow(x))
set.seed(123) # seed is necessary to make it reproducible
train_ind <- sample(seq_len(nrow(x)), size = sample_size)
train_set <- x[train_ind, ]
test <- x[-train_ind, ]
train <- train_set[,1:2]

points <- train
k <- 3

#res_clusters <- my_kmeans(points,k,"Mahalanobis")
#clusters <- res_clusters[["clusters"]]
#centers <- res_clusters[["centers"]]
#idx <- clusters[,ncol(clusters)]
#dist_functions <- c("Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")
#for (i in seq(along=idx)){
#  a<-switch(idx[i],"red","green","blue") #num of colours
#  plot(points[i,1],points[i,2], col=a,type="p",xlim=c(-10,20),ylim=c(-10,20))
#  par(new=TRUE)
#}
#cluster_1 = clusters[clusters[,3] == 1,1:2]
#cluster_2 = clusters[clusters[,3] == 2,1:2]
#cluster_3 = clusters[clusters[,3] == 3,1:2]
#cov_cluster_1 = cov(cluster_1)
#cov_cluster_2 = cov(cluster_2)
#cov_cluster_3 = cov(cluster_3)
#drawEllipse(centers[1,1:2], cov_cluster_1, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
#drawEllipse(centers[2,1:2], cov_cluster_2, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
#drawEllipse(centers[3,1:2], cov_cluster_3, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))

dist_functions <- c("Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")

#test_funcs <- c(1:length(dist_functions))
test_funcs <- c(7:7)

for(i in test_funcs){
  dist_func <- switch(i,"Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")
  res_clusters <- my_kmeans(points,k,dist_func)
  clusters <- res_clusters[["clusters"]]
  centers <- res_clusters[["centers"]]
  idx <- clusters[,ncol(clusters)]
  for (i in seq(along=idx)){
    a<-switch(idx[i],"red","green","blue")#,"yellow") #num of colours
    plot(points[i,1],points[i,2], col=a,type="p",xlim=c(-10,20),ylim=c(-10,20),sub=dist_func, main="k-Means",xlab="x",ylab="y")
    par(new=TRUE)
  }
  cluster_1 = clusters[clusters[,3] == 1,1:2]
  cluster_2 = clusters[clusters[,3] == 2,1:2]
  cluster_3 = clusters[clusters[,3] == 3,1:2]
  #cluster_4 = clusters[clusters[,3] == 4,1:2]
  cov_cluster_1 = cov(cluster_1)
  cov_cluster_2 = cov(cluster_2)
  cov_cluster_3 = cov(cluster_3)
  #cov_cluster_4 = cov(cluster_4)
  drawEllipse(centers[1,1:2], cov_cluster_1, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
  drawEllipse(centers[2,1:2], cov_cluster_2, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
  drawEllipse(centers[3,1:2], cov_cluster_3, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
  #drawEllipse(centers[4,1:2], cov_cluster_3, radius=3, nv = 100, axes = FALSE, fg = par('fg'), bg = NA, colCtr = "red", lty = par('lty'), lwd = par('lwd'), pch = par('pch'), cex = par('cex'))
  
}

int_ratios_matrix <- matrix(c(0),nrow=length(dist_functions),ncol=k+1)
rownames(int_ratios_matrix) <- dist_functions
colnames(int_ratios_matrix) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Average")

silh_coeff_matrix <- matrix(c(0),nrow=length(dist_functions),ncol=3)
rownames(silh_coeff_matrix) <- dist_functions
colnames(silh_coeff_matrix) <- c("Number of SC >= 0", "Number of SC < 0", "Average value")

for(i in test_funcs){
  dist_func <- switch(i,"Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")
  res_clusters <- my_kmeans(points,k,dist_func)
  clusters <- res_clusters[["clusters"]]
  int_ratios_matrix[i,1:k] <- int_dist_ratio(clusters,k)
  int_ratios_matrix[i,k+1] <- my_mean(int_ratios_matrix[i,1:k])
  
  silh_vect <- silhoette_func(clusters)
  silh_coeff_matrix[i,1] <- length(silh_vect[silh_vect >= 0])
  silh_coeff_matrix[i,2] <- length(silh_vect[silh_vect < 0])
  silh_coeff_matrix[i,3] <- sum(silh_vect) / length(silh_vect)
}

print(int_ratios_matrix)
print(silh_coeff_matrix)



