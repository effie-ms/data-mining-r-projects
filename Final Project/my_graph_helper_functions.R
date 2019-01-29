binomial_coefficient <- function(n, k){
  return(factorial(n)/(factorial(k)*factorial(n-k)))
}

get_node_set_power <- function(nodes_idxs_vector){
  return(length(nodes_idxs_vector))
}

get_network_power <- function(adjacency_matrix){
  return(nrow(adjacency_matrix))
}

get_set_of_inbound_nodes_idxs <- function(adjacency_matrix, node_idx){
  inbound_nodes <- adjacency_matrix[,node_idx]
  inbound_nodes_idxs <- c()
  for(i in 1:length(inbound_nodes)){
    if(inbound_nodes[i] != 0 && i != node_idx){
      inbound_nodes_idxs <- c(inbound_nodes_idxs, i)
    }
  }
  return(inbound_nodes_idxs)
}

get_set_of_outbound_nodes_idxs <- function(adjacency_matrix, node_idx){
  outbound_nodes <- adjacency_matrix[node_idx,]
  outbound_nodes_idxs <- c()
  for(i in 1:length(outbound_nodes)){
    if(outbound_nodes[i] != 0 && i != node_idx){
      outbound_nodes_idxs <- c(outbound_nodes_idxs, i)
    }
  }
  return(outbound_nodes_idxs)
}

get_degree <- function(adjacency_matrix, i_idx){
  degree <- get_node_set_power(get_set_of_inbound_nodes_idxs(adjacency_matrix, i_idx))
  return(degree)
}

get_indegree <- function(adjacency_matrix, i_idx){
  degree <- get_node_set_power(get_set_of_inbound_nodes_idxs(adjacency_matrix, i_idx))
  return(degree)
}

get_outdegree <- function(adjacency_matrix, i_idx){
  degree <- get_node_set_power(get_set_of_outbound_nodes_idxs(adjacency_matrix, i_idx))
  return(degree)
}

#Find the vertex of minimum distance not included yet in shortest path tree 
get_min_distance_vertex_idx <- function(dist, included_in_path_vector, graph_power) { 
  min <- Inf
  min_idx <- 0
  for (v in 1:graph_power) 
    if (included_in_path_vector[v] == FALSE && dist[v] <= min){
      min <- dist[v]
      min_idx <- v
    }
  return(min_idx)
} 

#Get the vector of shortest distances from src to all of the nodes (Dijkstra algorithm)
get_shortest_distances_dijkstra <- function(adjacency_matrix, src) { 
  graph_power <- get_network_power(adjacency_matrix)
  #Initialization
  dist <- c()
  included_in_path_vector <- c() 
  for (i in 1:graph_power) {
    dist[i] <- Inf
    included_in_path_vector[i] <- FALSE 
  }
  dist[src] <- 0
  # Find shortest path for all vertices 
  for (count in 1:graph_power-1) { 
    u <- get_min_distance_vertex_idx(dist, included_in_path_vector, graph_power) #min distance, unprocessed vertex
    included_in_path_vector[u] <- TRUE
    for (v in 1:graph_power) {
      if (included_in_path_vector[v] == FALSE && adjacency_matrix[u,v] > 0 && dist[u] != Inf && dist[u] + adjacency_matrix[u,v] < dist[v]) {
        dist[v] <- dist[u] + adjacency_matrix[u,v]
      }
    }
  }
  return(dist)
}

get_all_paths <- function(u, d, visited, path, adjacency_matrix, all_paths){ 
  visited[u] <- TRUE
  path <- c(path,u) 
  if (u == d){
    all_paths[[length(all_paths)+1]] <- path
  } 
  else{ 
    for (i in get_set_of_outbound_nodes_idxs(adjacency_matrix, u)){
      if (visited[i]==FALSE){
        all_paths <- get_all_paths(i, d, visited, path, adjacency_matrix, all_paths) 
      }
    }
  }
  path <- path[-length(path)]
  visited[u]<- FALSE
  return(all_paths)
}

# find paths from s to d
find_all_paths <- function(s, d, adjacency_matrix){
  visited <- c()
  for(i in 1:nrow(adjacency_matrix)){
    visited <- c(visited, FALSE)
  }
  path <- c()
  all_paths <- list()
  all_paths <- get_all_paths(s, d,visited, path, adjacency_matrix, all_paths) 
  return(all_paths)
} 

get_path_distance <- function(path, adjacency_matrix){
  dist <- 0
  for(i in 2:length(path)){
    dist <- dist + adjacency_matrix[path[i-1],path[i]]
  }
  return(dist)
}

get_shortest_paths <- function(s, d, adjacency_matrix){
  min_dist <- get_shortest_distances_dijkstra(adjacency_matrix, s)[d]
  all_paths <- find_all_paths(s,d,adjacency_matrix)
  dist <- c()
  for(i in 1:length(all_paths)){
    dist[i] <- get_path_distance(all_paths[[i]], adjacency_matrix)
  }
  shortest_paths <- list()
  for(i in 1:length(all_paths)){
    if(dist[i] == min_dist){
      shortest_paths[[length(shortest_paths)+1]] <- all_paths[[i]]
    }
  }
  return(shortest_paths)
}

get_shortest_paths_through_i_count <- function(adjacency_matrix, j_idx, k_idx, i_idx){
  all_shortest_paths <- get_shortest_paths(j_idx, k_idx, adjacency_matrix)
  shortest_paths_through_i_count <- 0
  for(i in 1:length(all_shortest_paths)){
    for(j in 1:length(all_shortest_paths[[i]])){
      if(all_shortest_paths[[i]][j] == i_idx){
        shortest_paths_through_i_count <- shortest_paths_through_i_count + 1
        break
      }
    }
  }
  return(shortest_paths_through_i_count)
} 

get_fraction_of_pairs_through_i <- function(adjacency_matrix, j_idx, k_idx, i_idx){
  qjk <-  length(get_shortest_paths(j_idx, k_idx, adjacency_matrix))
  qjki <- get_shortest_paths_through_i_count(adjacency_matrix, j_idx, k_idx, i_idx)
  fjki <- qjki / qjk # the fraction of pairs that pass through the node i
  return(fjki)
}

get_intersection_set_count <- function(set1, set2){
  intersection_counter <- 0
  for(i in 1:length(set1)){
    for(j in 1:length(set2)){
      if(set1[i] == set2[j]){
        intersection_counter <- intersection_counter + 1
        break
      }
    }
  }
  return(intersection_counter)
}

