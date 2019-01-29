get_activity <- function(activities_dictionary, points){
  res <- c()
  for(k in 1:length(points)) {
    for(i in 1:nrow(activities_dictionary)){
      if(as.numeric(activities_dictionary[i,2]) == points[k]){
        res[k] <- activities_dictionary[i,1]
        break
      }
    }
  }
  return(res)
}

get_ts_list <- function(ts_all_matrix, labels_participants_unique, labels_activities_unique){
  ts_all <- list()
  k <- 1
  for(i in 1:length(labels_participants_unique)){
    participant <- labels_participants_unique[i]
    for(j in 1:length(labels_activities_unique)) {
      activity <- labels_activities_unique[j]
      temp <- ts_all_matrix[ts_all_matrix[,ncol(ts_all_matrix)-1] == participant, ]
      temp <- temp[temp[,ncol(temp)] == activity, ]
      if(nrow(temp) != 0) {
        ts_all[[k]] <- temp
        k <- k+1
      }
    }
  }
  ts_list <- list()
  for(i in 1:length(ts_all)){
    ts <- list(
      participant = ts_all[[i]][1,(ncol(ts_all[[i]])-1)],
      points = ts_all[[i]][,1:(ncol(ts_all[[i]])-2)],
      activity = ts_all[[i]][[1,(ncol(ts_all[[i]]))]])
    ts_list[[i]] <- ts
  }
  return(ts_list)
}
