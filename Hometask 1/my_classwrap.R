#####-----my_classwrap.R-----#####

#Get points of the jth class
get_class_points <- function(classified_points, j){
  class_points_j <- c()
  for(i in 1:nrow(classified_points)){
    if(classified_points[i,ncol(classified_points)]==j){
      class_points_j <- rbind(class_points_j,classified_points[i,])
    }
  }
  return(class_points_j)
}

#Get Fischer score of the features of classified points
my_fischer_score <- function(classified_points,n_classes){
  entire_mean <- my_mean_dataset(classified_points[,1:(ncol(classified_points)-1)])
  f_numeraror <- c(0)
  f_denominator <- c(0)
  for(j in 1:n_classes){
    class_points_j <- get_class_points(classified_points,j)
    mean_j <- my_mean_dataset(class_points_j[,1:(ncol(class_points_j)-1)])
    fraction_points_j <- nrow(class_points_j)/nrow(classified_points)
    
    f_numeraror <- f_numeraror + (fraction_points_j * (mean_j - entire_mean)^2)
    f_denominator <- f_denominator + (fraction_points_j * my_variance_dataset(class_points_j[,1:(ncol(class_points_j)-1)]))
  }
  f <- f_numeraror / f_denominator
  return(f)
}

#Get the accuracy of the classification
my_accuracy <- function(classified_points, known_points){
  true_values_count <- 0
  for(i in 1:nrow(classified_points)){
    if(classified_points[i,ncol(classified_points)] == known_points[i,ncol(known_points)]){
      true_values_count <- true_values_count + 1
    }
  }
  return(true_values_count / nrow(classified_points))
}

#####-----End of my_classwrap.R-----#####

