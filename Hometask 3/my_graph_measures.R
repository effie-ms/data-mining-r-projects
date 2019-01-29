source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 3/my_graph_helper_functions.R")

#  1. Local clustering coefficient.
get_clustering_coefficient <- function(adjacency_matrix, i_idx){
  s <- get_set_of_inbound_nodes_idxs(adjacency_matrix, i_idx) #for undirected graph
  s <- c(s,i_idx) #include the node itself
  n <- get_node_set_power(s)
  edges_counter <- 0
  for(j in 1:n){
    node_idx1 <- s[j]
    k <- j + 1
    while(k <= n){
      node_idx2 <- s[k]
      if(adjacency_matrix[node_idx1,node_idx2] != 0){
        edges_counter <- edges_counter + 1
      }
      k <- k+1
    }
  }
  nju <- edges_counter / binomial_coefficient(n, 2)
  return(nju)
}

get_average_clustering_coefficient <- function(adjacency_matrix){
  clustering_coefficients_vector <- c()
  for(i in 1:nrow(adjacency_matrix)){
    clustering_coefficient <- get_clustering_coefficient(adjacency_matrix, i)
    clustering_coefficients_vector <- c(clustering_coefficients_vector, clustering_coefficient)
  }
  return(sum(clustering_coefficients_vector)/length(clustering_coefficients_vector))
}

#2. Degree centrality.
get_degree_centrality <- function(adjacency_matrix, i_idx){
  n <- get_network_power(adjacency_matrix)
  degree_centrality <- get_degree(adjacency_matrix, i_idx) / (n-1)
  return(degree_centrality)
}

#3. Degree prestige.
get_degree_prestige <- function(adjacency_matrix, i_idx){
  n <- get_network_power(adjacency_matrix)
  degree_prestige <- get_indegree(adjacency_matrix, i_idx) / (n-1)
  return(degree_prestige)
}

#4. Gregariousness of a node.
get_gregariousness <- function(adjacency_matrix, i_idx){
  n <- get_network_power(adjacency_matrix)
  gregariousness <- get_outdegree(adjacency_matrix, i_idx) / (n-1)
  return(gregariousness)
}

#5. Closeness centrality and proximity prestige.
get_average_shortest_path_distance <- function(adjacency_matrix, i_idx){
  dijkstra <- get_shortest_distances_dijkstra(adjacency_matrix, i_idx)
  dijkstra <- dijkstra[dijkstra != Inf]
  n <- length(dijkstra)
  avdist <- sum(dijkstra) / (n-1)
  return(avdist)
}

get_closeness_centrality <- function(adjacency_matrix, i_idx){
  average_shortest_path_distance <- get_average_shortest_path_distance(adjacency_matrix, i_idx)
  return(1 / average_shortest_path_distance)
}

get_influence_set <- function(adjacency_matrix, i_idx){
  dijkstra <- get_shortest_distances_dijkstra(adjacency_matrix, i_idx)
  dijkstra <- dijkstra[dijkstra != Inf]
  return(dijkstra)
}

get_influence_fraction <- function(influence_set, n){
  return(length(influence_set) / (n-1))
}

get_proximity_prestige <- function(adjacency_matrix, i_idx){
  influence_set <- get_influence_set(adjacency_matrix, i_idx) #the set of nodes that can reach node i with a directed path
  n <- get_network_power(adjacency_matrix)
  influence_fraction <- get_influence_fraction(influence_set, n)
  avdist <- get_average_shortest_path_distance(adjacency_matrix, i_idx)
  return(influence_fraction / avdist)
}

#6. Betweenness Centrality
get_betweenness_centrality <- function(adjacency_matrix, i_idx){
  n <- get_network_power(adjacency_matrix)
  btw_centrality <- 0
  for(j in 1:n){
    k <- j+1 
    while(k <= n){
      fjki <- get_fraction_of_pairs_through_i(adjacency_matrix, j, k, i_idx)
      btw_centrality <- btw_centrality + fjki
      k <- k+1
    }
  }
  return(btw_centrality / binomial_coefficient(n,2))
}

#7. Common neighbor based measure.
get_common_neighbor_based_measure <- function(adjacency_matrix, i_idx, j_idx){
  si <- get_set_of_inbound_nodes_idxs(adjacency_matrix, i_idx)
  sj <- get_set_of_inbound_nodes_idxs(adjacency_matrix, j_idx)
  common_neighbor_counter <- get_intersection_set_count(si,sj)
  return(common_neighbor_counter)
}

#8. Jaccard Measure.
get_jaccard_measure <- function(adjacency_matrix, i_idx, j_idx){
  si <- get_set_of_inbound_nodes_idxs(adjacency_matrix, i_idx)
  sj <- get_set_of_inbound_nodes_idxs(adjacency_matrix, j_idx)
  intersection_count <- get_intersection_set_count(si,sj)
  join_count <- length(si) + length(sj) - intersection_count
  return(intersection_count/join_count)
}
