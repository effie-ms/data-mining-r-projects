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

elt1 <- c(0,0)
elt2 <- c(0,0)

d <- length(x)
z1<-matrix(c(0),d,d)

colours <- c("red","orange","yellow","green","blue","violet")
for(k in 0:5){
  if (k == 0){
    xn <- x
    yn <- y
  }
  else {
    phase <- pi/6
    angle <- phase * k
    xn <- x*cos(angle)+y*sin(angle)
    yn <- y*cos(angle)-x*sin(angle)
    #x <- xn
    #y <- yn
  }
  dataset <-cbind(xn,yn)
  
  for (i in 1:d){
    elt2[1] <- xn[i]
    for(j in 1:d){
      elt2[2] <- yn[j]
      z1[i,j]<-my_dist_func(elt1,elt2,"Mahalanobis", dataset[,1:2])
    }
  }
  
  if (k == 0) {
    persp3d(x, y, z1,alpha=0.5, col=colours[k+1])
  }
  else{
    persp3d(x, y, z1,alpha=0.5, col=colours[k+1],add=T)
  }

}
