# clear everything
rm(list=ls())

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_matlib.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_knn.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_classwrap.R")
load(file="D:/Effie/TTU/Data Mining and Network Analysis/Practices/kNN_data1.RData")

# counters for two sets
k_train<-0
k_valid<-0
# create the arrays to store training and validation data
x_train<-matrix(c(0),1000,3)
x_valid<-matrix(c(0),500,3)

# split the data into two sets
for(i in seq(along=x[,1])){
  if (i%%3==0){
    k_valid<-k_valid+1
    x_valid[k_valid,]<-x[i,]
  }
  else{
    k_train<-k_train+1
    x_train[k_train,]<-x[i,]
  }
}

#dist_functions <- c("Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")

#coeff_matrix <- matrix(c(0),nrow=length(dist_functions),ncol=3)
#rownames(coeff_matrix) <- dist_functions
#colnames(coeff_matrix) <- c("Accuracy", "Fischer score x", "Fischer score y")

#for(i in 1:length(dist_functions)){
#  dist_func <- switch(i,"Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")
#  classified_points <- my_knn(x_train, x_valid[,1:2], 3, dist_func)
#  predicted_classes<-classified_points[,ncol(classified_points)]
#  coeff_matrix[i,1] <- my_accuracy(classified_points, x_valid)
#  coeff_matrix[i,2:3] <- my_fischer_score(classified_points, 3)
#}
#print(coeff_matrix)

coeff_matrix <- matrix(c(0),nrow=length(2:10),ncol=3)
rownames(coeff_matrix) <- c("k=2","k=3","k=4","k=5","k=6","k=7","k=8","k=9","k=10")
colnames(coeff_matrix) <- c("Accuracy", "Fischer score x", "Fischer score y")

for(i in 2:2){
  classified_points <- my_knn(x_train, x_valid[,1:2], i, "Mahalanobis")
  predicted_classes<-classified_points[,ncol(classified_points)]
  coeff_matrix[i-1,1] <- my_accuracy(classified_points, x_valid)
  coeff_matrix[i-1,2:3] <- my_fischer_score(classified_points, 3)
}
print(coeff_matrix)




