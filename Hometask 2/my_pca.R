#####-----my_pca.R-----#####

# Principal component analysis (from the 1st practice)
my_pca <- function(dataset){
  # compute the correlations between terms
  #correlation_matrix <- cor(dataset)
  # compute standard deviations
  #std_vector <- c()
  #for(i in 1:ncol(dataset)){
  #  std_vector <- c(std_vector, sd(dataset[,i]))
  #}
  # find mean
  mean_vector <- c()
  for(i in 1:ncol(dataset)){
    mean_vector <- c(mean_vector, mean(dataset[,i]))
  }
  # remove mean (center the data)
  for (i in 1:ncol(dataset)){
    dataset[,i] <- dataset[,i] - mean_vector[i]
  }
  D <- dataset
  # perform PCA
  # find covariance matrix
  cov_D<-cov(D)
  #find it eigenvalues and eigenvectors
  eig_cov_D<-eigen(cov_D)
  # compute D prime (D rotated into the new coordinates)
  rotated_D<-D%*%eig_cov_D$vectors
  return(rotated_D)
}

#####-----End of my_pca.R-----#####