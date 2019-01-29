#####-----my_matlib.R-----#####

#Returns the sum of my_vector's elements
my_sum <- function(my_vector){
  sum_res <- 0
  for(i in 1:length(my_vector)){
    sum_res <- sum_res + my_vector[i]
  }
  return(sum_res)
}

#Returns the mean of my_vector
my_mean <- function(my_vector){
  mean_res <- my_sum(my_vector) / length(my_vector)
  return(mean_res)
}

#Return the variance of my_vector
my_variance<-function(my_vector){
  n <- length(my_vector)
  mean_vect <- my_mean(my_vector)
  sqsum <- my_sum((my_vector - mean_vect)^2)
  variance_res <- (1/(n-1)) * sqsum
  return(variance_res)
}

my_covariance<-function(my_vector1,my_vector2, allData1, allData2){
  n <- length(my_vector1)
  sqsum <- my_sum((my_vector1 - my_mean(allData1))*(my_vector2 - my_mean(allData2)))
  covariance_res <- (1/(n-1)) * sqsum
  return(covariance_res)
}

my_covariance_matrix<-function(input_matrix){
  res_matrix <- matrix(c(0),ncol(input_matrix),ncol(input_matrix))
  for(i in 1:ncol(input_matrix)){
    vect1 <- input_matrix[,i]
    for(j in 1:ncol(input_matrix)){
      vect2 <- input_matrix[,j]
      res_matrix[i,j] <- my_covariance(vect1,vect2,vect1,vect2)
    }
  }
  return(res_matrix)
}  

my_covariance_matrix_full<-function(point1, point2, allData){
  res_matrix <- matrix(c(0),length(point1),length(point1))
  for(i in 1:length(point1)){
    vect1 <- allData[,i]
    for(j in 1:length(point1)){
      vect2 <- allData[,j]
      res_matrix[i,j] <- my_covariance(point1, point2, vect1,vect2)
    }
  }
  return(res_matrix)
}


inverse_matrix<-function(my_matrix){
  matr_det <- det(my_matrix)
  if(matr_det != 0 & abs(matr_det) > 1e-6){
    return(solve(my_matrix))
  }
  else{
    return(NaN)
  }
}

equal_vectors <- function(vector1, vector2){
  d <- length(vector1)
  equal <- TRUE
  for(i in 1:d){
    if(vector1[i] != vector2[i]){
      equal <- FALSE
    }
  }
  return(equal)
}

my_mean_dataset <- function(dataset){
  res_mean <- c()
  for(i in 1:ncol(dataset)){
    res_mean[i] <- my_mean(dataset[,i])
  }
  return(res_mean)
}

my_variance_dataset <- function(dataset){
  res_variance <- c()
  for(i in 1:ncol(dataset)){
    res_variance[i] <- my_variance(dataset[,i])
  }
  return(res_variance)
}

my_standard_deviation_dataset <- function(dataset){
  return(sqrt(my_variance_dataset(dataset)))
}

#####-----End my_matlib.R-----#####

# For testing purposes (for comparison with standard functions)
#vect <- c(1,2,3,6,4)
#vect1 <- c(5,-6,-7,0,9)
#vect2 <- c(4,6,7,2,4)

#print(my_sum(vect))
#print(sum(vect))

#print(my_mean(vect))
#print(mean(vect))

#print(my_variance(vect))
#print(var(vect))

#print(my_covariance(vect, vect1,vect,vect1))
#print(cov(vect,vect1))

#m <- rbind(vect1, vect2)
#covm <- cov(m)
#print(covm)
#cov_m <- my_covariance_matrix(m)
#print(cov_m)

#print(det(cov_m))

#inv_matr <- inverse_matrix(cov_m)
#print(inv_matr)

#mmeans <- colMeans(m)
#D2<-mahalanobis(m,mmeans,covm)
#print(D2)
