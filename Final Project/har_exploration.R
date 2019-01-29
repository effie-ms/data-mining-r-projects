rm(list = ls())

# Load datasets
train <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/train.csv",header=T)
test <- read.csv("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/test.csv",header=T)

data <- rbind(train,test)
names(data) <- make.names(names(data),unique=TRUE)
dim(data)

# Principal Component Analysis to reduce the number of dimensions
train_pca <- prcomp(train[,1:561], center=TRUE, scale = TRUE) #built-in R stats package
train_activities_labels <- as.vector(train[,563])
train_participants_labels <- as.vector(train[,562])
train_pca_rotated_coordinates <- train_pca[["x"]]
train_pca_variances <- train_pca[["sdev"]]^2
train_pca_relative_variances <- train_pca_variances/sum(train_pca_variances)

train_pca_relative_variances_100 <- cbind(c(1:100), train_pca_relative_variances[1:100])
colnames(train_pca_relative_variances_100) <- c("The order number of a principal component", "Relative variance")
train_pca_relative_variances_df <- as.data.frame(train_pca_relative_variances_100)

library(ggplot2)    
ggplot(train_pca_relative_variances_df, aes(train_pca_relative_variances_df[,1], train_pca_relative_variances_df[,2])) +
  scale_x_continuous("The order number of a principal component") +
  scale_y_continuous("Relative variance") +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Principal Components Relative Variances Bar Chart")

# Plotting Cummulative proportions of Principal Components to decide number of components to be taken into consideration
cumulative_sums_relative_variences <- cumsum(train_pca_relative_variances)
cumulative_sums_relative_variences <- cbind(c(1:length(cumulative_sums_relative_variences)), cumulative_sums_relative_variences)
colnames(cumulative_sums_relative_variences) <- c("Principal component", "Cumulative Proportion of variance explained")

cumulative_sums_relative_variences_df <- as.data.frame(cumulative_sums_relative_variences)
ggplot(data=cumulative_sums_relative_variences_df, aes(x=cumulative_sums_relative_variences_df[,1], y=cumulative_sums_relative_variences_df[,2], group=1)) +
  scale_x_continuous("The number of principal components") +
  scale_y_continuous("Cumulative proportion of variance explained") +
  geom_line()+
  geom_point()+
  labs(title = "Principal components proportions")

# First 100 principal components explain 95% of variance in dataset
# Selecting first 100 principal components 
train_pca <- as.matrix(train_pca_rotated_coordinates)
train_pca_activities <- cbind(train_pca[,1:100],train_activities_labels)
train_pca_participants <- cbind(train_pca[,1:100],train_participants_labels)

#Dataset Exploration
# Pie Chart with Percentages
# Get distinct labels of activities
activity_labels <- unique(train_activities_labels)
activity_slices <- c()
for(i in 1:length(activity_labels)){
  activity_slices[i] <- length(train_pca_activities[train_pca_activities[,101] == activity_labels[i]])
}
activity_percentages <- round(activity_slices/sum(activity_slices)*100)
activity_labels <- paste(activity_labels, activity_percentages) # add percentages to labels 
activity_labels <- paste(activity_labels,"%",sep="") # add % to labels 
pie(activity_slices,labels = activity_labels, col=rainbow(length(activity_labels)),main="Pie Chart of Activities")

plot_data_all <- cbind(train_pca[,1], train_participants_labels, train_activities_labels)
train_activities_labels <- unique(train_activities_labels)
for(j in 1:length(train_activities_labels)){ #for each type of activity
  activity_label <- train_activities_labels[j]
  plot_data_activity <- plot_data_all[plot_data_all[,3] == activity_label,]
  activity_participants <- list()
  for(i in 1:nrow(plot_data_activity)){ #for each record of a certain activity type
    activity_participants[[plot_data_activity[i,2]]] <- c(as.numeric(activity_participants[[plot_data_activity[i,2]]]), as.numeric(plot_data_activity[i,1]))
  }
  lengthes_x <- c()
  for(i in 1:5){
    lengthes_x <- c(lengthes_x, length(activity_participants[[i]]))
  }
  max_length_x <- max(lengthes_x)
  max_ys <- c()
  min_ys <- c()
  for(i in 1:5){
    max_ys <- c(max_ys, as.numeric(max(activity_participants[[i]])))
    min_ys <- c(min_ys, as.numeric(min(activity_participants[[i]])))
  }
  max_length_y <- as.numeric(max(max_ys))
  min_length_y <- as.numeric(min(min_ys))
  
  first_ts_vect <- array(as.numeric(unlist(activity_participants[1])))
  plot(first_ts_vect,type = "o",col = 1, xlab = "index", ylab = "PC1", main = paste("Activity participants time series - ", train_activities_labels[j]), xlim = c(-10, max_length_x), ylim = c(min_length_y, max_length_y))
  for(i in 2:5){
    ts_arr <- array(as.numeric(unlist(activity_participants[i])))
    lines(ts_arr, type = "o", col = i)
    legend(-10, max_length_y, legend=names(activity_participants)[1:5], col=c(1:5), lty=1:2, cex=0.8)
  }
}