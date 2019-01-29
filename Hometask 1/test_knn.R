rm(list=ls())
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_knn.R")
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

dist_functions <- c("Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")

for(i in 1:2){
  dist_func <- switch(i,"Minkowski1","Minkowski2","Minkowski3","Minkowski4","Minkowski5","Canberra","Mahalanobis","Chebyshev","Cosine")
  classified_points <- my_knn(x_train, x_valid[,1:2], 3, dist_func)
  predicted_classes<-classified_points[,ncol(classified_points)]
  for(i in seq(along=x_valid[,1])){
    a<-switch(x_valid[i,3], "red","green","blue")
    b<-switch(predicted_classes[i], 'red','green','blue')
    plot(x_valid[i,1],x_valid[i,2],col=a,type="p",cex=2,xlim=c(-10,20),ylim=c(-10,20),sub=dist_func, main="kNN",xlab="x",ylab="y")
    par(new=TRUE)
    plot(x_valid[i,1],x_valid[i,2],col=b,type="p",pch=8,xlim=c(-10,20),ylim=c(-10,20),sub=dist_func, main="kNN",xlab="x",ylab="y")
    par(new=TRUE)
  }
}






