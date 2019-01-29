source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Final Project/my_graph_helper_functions.R")

contains_item <- function(arr,item){
  for(i in 1:length(arr)){
    if(arr[i] == item){
      return(TRUE)
    }
  }
  return(FALSE)
}

get_internal_cost <- function(adjacency_matrix,node,n1,n2){
  incident_nodes <- get_set_of_inbound_nodes_idxs(adjacency_matrix, node)
  n_node <- 2
  if(contains_item(n1,node) == TRUE){
    n_node <- 1
  }
  incident_same_partition <- c()
  for(i in 1:length(incident_nodes)){
    if(n_node == 1){
      if(contains_item(n1,incident_nodes[i]) == TRUE){
        incident_same_partition <- c(incident_same_partition, adjacency_matrix[node,incident_nodes[i]])
      }
    }
    else{
      if(contains_item(n2,incident_nodes[i]) == TRUE){
        incident_same_partition <- c(incident_same_partition, adjacency_matrix[node,incident_nodes[i]])
      }
    }
  }
  return(sum(incident_same_partition))
}

get_external_cost <- function(adjacency_matrix,node,n1,n2){
  incident_nodes <- get_set_of_inbound_nodes_idxs(adjacency_matrix, node)
  n_node <- 2
  if(contains_item(n1,node) == TRUE){
    n_node <- 1
  }
  incident_other_partition <- c()
  for(i in 1:length(incident_nodes)){
    if(n_node == 1){
      if(contains_item(n1,incident_nodes[i]) == FALSE){
        incident_other_partition <- c(incident_other_partition, adjacency_matrix[node,incident_nodes[i]])
      }
    }
    else{
      if(contains_item(n2,incident_nodes[i]) == FALSE){
        incident_other_partition <- c(incident_other_partition, adjacency_matrix[node,incident_nodes[i]])
      }
    }
  }
  return(sum(incident_other_partition))
}

get_moving_node_gain <- function(adjacency_matrix,node,n1,n2){
  e <- get_external_cost(adjacency_matrix,node,n1,n2)
  i <- get_internal_cost(adjacency_matrix,node,n1,n2)
  return(e-i)
}

get_exchanging_node_gain <- function(adjacency_matrix,node1,node2,n1,n2){
  d1 <- get_moving_node_gain(adjacency_matrix,node1,n1,n2)
  d2 <- get_moving_node_gain(adjacency_matrix,node2,n1,n2)
  j <- d1 + d2 - 2*adjacency_matrix[node1,node2]
  return(j)
}

find_node_pair_with_highest_exchange_gain <- function(adjacency_matrix,n1,n2,marks){
  all_pairs <- c()
  for(i in 1:length(n1)){
    if(marks[1,n1[i]] == 0 && marks[2,n1[i]] == 0){
      for(j in 1:length(n2)){
        if(marks[2,n2[j]] == 0 && marks[1,n2[j]] == 0){
          all_pairs <- rbind(all_pairs, c(n1[i],n2[j],get_exchanging_node_gain(adjacency_matrix,n1[i],n2[j],n1,n2)))
        }
      }
    }
  }
  if(is.null(all_pairs) == FALSE && nrow(all_pairs) > 1){
    all_pairs <- all_pairs[order(all_pairs[,3], decreasing = TRUE),]
    max_gain_pair <- all_pairs[1,1:3]
    return(max_gain_pair)
  }
  else{
    return(all_pairs)
  }
}

exchange_nodes <- function(arr, to_be_changed, change_for){
  for(i in 1:length(arr)){
    if(arr[i] == to_be_changed){
      arr[i] <- change_for
      break
    }
  }
  return(arr)
}

get_pair <- function(marks,i){
  idx1 <- 0
  for(j1 in 1:ncol(marks)){
    if (marks[1,j1] == i){
      idx1 <- j1
      break
    }
  }
  idx2 <- 0
  for(j2 in 1:ncol(marks)){
    if (marks[2,j2] == i){
      idx2 <- j2
      break
    }
  }
  return(c(idx1,idx2))
}

get_max_k <- function(gains){
  k_sum_vector <- c()
  for(i in 1:length(gains)){
    k_sum_vector[i] <- sum(gains[1:i])
  }
  max <- -Inf
  k <- 0
  for(i in 1:length(k_sum_vector)){
    if(max < k_sum_vector[i]){
      max <- k_sum_vector[i]
      k <- i
    }
  }
  if(max < 0){
    k <- 0-k
  }
  return(k)
}

get_clusters_Kernighan_Lin <- function(adjacency_matrix){
  #Create random initial partition of N into N1 and N2
  n <- nrow(adjacency_matrix)
  set.seed(123)
  n1_idxs <- sample(seq_len(n), size = n/2)
  n2_idxs <- c(1:n)[-n1_idxs]
  c <- 0
  nn <- n/2
  while(c < nn){
    #Unmark all nodes in N
    marks <- matrix(c(0),2,n)
    gains <- c()
    new_n1_vector <- n1_idxs
    new_n2_vector <- n2_idxs
    for(i in 1:nn){
      #Select xi from N1 and yi from N2 to be the unmarked node pair with the highest exchange-gain
      pair <- find_node_pair_with_highest_exchange_gain(adjacency_matrix,new_n1_vector,new_n2_vector, marks)
      #Mark xi and yi
      marks[1,pair[1]] <- i
      marks[2,pair[2]] <- i
      gains[i] <- pair[3]
      new_n1_vector <- exchange_nodes(new_n1_vector, pair[1], pair[2])
      new_n2_vector <- exchange_nodes(new_n2_vector, pair[2], pair[1])
    }
    #Determine k that maximizes Gk
    k <- get_max_k(gains)
    if(k > 0){
      for(i in 1:k){
        #Exchange
        pair <- get_pair(marks,i)
        n1_idxs <- exchange_nodes(n1_idxs, pair[1], pair[2])
        n2_idxs <- exchange_nodes(n2_idxs, pair[2], pair[1])
      }
    }
    else {
      break
    }
    c <- c + 1
  }
  return(rbind(n1_idxs, n2_idxs))
}
