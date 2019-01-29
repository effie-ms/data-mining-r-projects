#####-----my_distfunc.R-----#####

my_dist_cosine <- function(vector1, vector2){
  return(sum(vector1*vector2) / (sqrt(sum(vector1^2)) * sqrt(sum(vector2^2))))
}

my_dist_minkowski <- function(point1, point2, order){
  return(sum(abs(point1-point2)^order)^(1/order))
}

my_dist_chebyshev <- function(point1,point2){
  return(max(abs(point1-point2)))
}

my_dist_canberra <- function(point1, point2){
  return(sum(abs(point1-point2) / (abs(point1) + abs(point2))))
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
