#####-----my_distfunc.R-----#####

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 1/my_matlib.R")

my_dist_minkowski <- function(point1, point2, order){
  dist <- my_sum(abs(point1-point2)^order)^(1/order)
  return(dist)
}

my_dist_chebyshev <- function(point1,point2){
  dist <- max(abs(point1-point2))
  return(dist)
}

my_dist_canberra <- function(point1, point2){
  dist <- my_sum(abs(point1-point2) / (abs(point1) + abs(point2)))
  return(dist)
}

my_dist_mahalanobis <- function(point1, point2, allData) {
  diff_col <- point1 - point2
  diff_row <- t(point1 - point2)
  dimensions <- length(point1)
  cov_m <- cov(allData)
  matr_det <- det(cov_m)
  if(matr_det != 0 & abs(matr_det) > 1e-6){
    inv_cov_matrix <- solve(cov_m)
    sqdist <- (diff_row  %*% inv_cov_matrix)  %*% diff_col
    dist <- sqrt(sqdist)
    return(dist)
  }
  else{
    return(my_dist_minkowski(point1,point2,2))
  }
}

my_dist_cosine <- function(point1, point2){
  dist <- my_sum(point1*point2) / (sqrt(my_sum(point1^2)) * sqrt(my_sum(point2^2)))
  return(dist)
}

my_dist_func <- function(point1, point2, method, points){
  dist <- 0
  if(method=="Minkowski1"){
    dist <- my_dist_minkowski(point1, point2, 1)
  }
  else if(method=="Minkowski2"){
    dist <- my_dist_minkowski(point1, point2, 2)
  }
  else if(method=="Minkowski3"){
    dist <- my_dist_minkowski(point1, point2, 3)
  }
  else if(method=="Minkowski4"){
    dist <- my_dist_minkowski(point1, point2, 4)
  }
  else if(method=="Minkowski5"){
    dist <- my_dist_minkowski(point1, point2, 5)
  }
  else if(method=="Canberra"){
    dist <- my_dist_canberra(point1, point2)
  }
  else if(method=="Mahalanobis"){
    dist <- my_dist_mahalanobis(point1, point2, points)
  }
  else if(method=="Chebyshev"){
    dist <- my_dist_chebyshev(point1, point2)
  }
  else if(method=="Cosine"){
    dist <- my_dist_cosine(point1, point2)
  }
  return(dist)
}

#####-----End of my_distfunc.R-----#####

# For testing purposes 
#vect <- c(4,7,5)
#vect1 <- c(2,6,0)
#print(my_dist_func(vect, vect1, "Minkowski1"))
#print(my_dist_func(vect, vect1, "Minkowski2"))
#print(my_dist_func(vect, vect1, "Minkowski3"))
#print(my_dist_func(vect, vect1, "Minkowski4"))
#print(my_dist_func(vect, vect1, "Minkowski5"))
#print(my_dist_func(vect, vect1, "Canberra"))
#print(my_dist_func(vect, vect1, "Chebyshev"))
#print(my_dist_func(vect, vect1, "Cosine"))
