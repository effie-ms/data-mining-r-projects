rm(list = ls())

t <- c(1:50) * 0.1
wx <- 1
wy <- 2

x <- c()
for(i in 1:25){
  x[i] <- sin(wx * t[i])
}
y <- c()
for(i in 1:15){
  y[i] <- sin(wy * t[i])
}

plot(x,type = "o",col = "red", xlab = "t", ylab = "sin(wt)", main = "DTW Test Plot")
lines(y, type = "o", col = "blue")

lengthX <- length(x)
lengthY <- length(y)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/dist_func.R")

DTW <- dtw_cost_matrix(x,y)
print(DTW)

path <- c(x[1],y[1])
t <- c(1,1)
lines(t, path, type = "o", col = "black")

i <- 2; j <- 2;
while (i < dim(DTW)[1] && j < dim(DTW)[2]){
  a<-min(DTW[i+1,j],DTW[i,j+1],DTW[i+1,j+1])
  if(a == DTW[i+1,j]){
    path <- c(x[i],y[j-1])
    t <- c(i,j-1)
    i <- i+1
  }else if(a == DTW[i,j+1]){
    path <- c(x[i-1],y[j])
    t <- c(i-1,j)
    j <- j+1
  }else{
    path <- c(x[i],y[j])
    t <- c(i,j)
    j <- j+1
    i <- i+1
  }
  lines(t, path, type = "o", col = "black")
}

path <- c(x[i], y[j])
t <- c(i, j)
lines(t, path, type = "o", col = "black")

print(i)
print(j)

while(i != lengthX || j != lengthY){
  if(i != lengthX){
    i <- i+1
  }
  else if (j != lengthY){
    j <- j+1
  }
  path <- c(x[i], y[j])
  t <- c(i, j)
  lines(t, path, type = "o", col = "black")
}

