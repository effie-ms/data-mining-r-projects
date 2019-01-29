# clear everything
rm(list=ls())

library("scatterplot3d")
library("car")
library("rgl")
library('lattice')
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_distfunc.R")

x<- c(-50:50)
x<-x*0.1
y<-x

dataset <- cbind(x,y)
  
elt1 <- c(0,0)
elt2 <- c(0,0)

d <- length(x)
z1<-matrix(c(0),d,d)
z2<-matrix(c(0),d,d)
z3<-matrix(c(0),d,d)
z4<-matrix(c(0),d,d)
z5<-matrix(c(0),d,d)
z6<-matrix(c(0),d,d)
z7<-matrix(c(0),d,d)
z8<-matrix(c(0),d,d)
z9<-matrix(c(0),d,d)


for (i in 1:d){
  elt2[1] <- x[i]
  for(j in 1:d){
    elt2[2] <- y[j]
    z1[i,j]<-my_dist_func(elt1,elt2,"Minkowski1",dataset[,1:2])
    z2[i,j]<-my_dist_func(elt1,elt2,"Minkowski2",dataset[,1:2])
    z3[i,j]<-my_dist_func(elt1,elt2,"Minkowski3",dataset[,1:2])
    z4[i,j]<-my_dist_func(elt1,elt2,"Minkowski4",dataset[,1:2])
    z5[i,j]<-my_dist_func(elt1,elt2,"Minkowski5",dataset[,1:2])
    z6[i,j]<-my_dist_func(elt1,elt2,"Canberra", dataset[,1:2])
    z7[i,j]<-my_dist_func(elt1,elt2,"Chebyshev",dataset[,1:2])
    z8[i,j]<-my_dist_func(elt1,elt2,"Cosine", dataset[,1:2])
    z9[i,j]<-my_dist_func(elt1,elt2,"Mahalanobis", dataset[,1:2])
  }
}
  
persp3d(x, y, z1,alpha=0.5, col="red")
persp3d(x, y, z2,alpha=0.5, col="orange",add=T)
persp3d(x, y, z3,alpha=0.5, col="yellow",add=T)
persp3d(x, y, z4,alpha=0.5, col="green",add=T)
persp3d(x, y, z5,alpha=0.5, col="blue",add=T)
persp3d(x, y, z6,alpha=0.5, col="gray",add=T)
persp3d(x, y, z7,alpha=0.5, col="violet",add=T)
persp3d(x, y, z8,alpha=0.5, col="pink",add=T)
persp3d(x, y, z9,alpha=0.5, col="brown",add=T)

  