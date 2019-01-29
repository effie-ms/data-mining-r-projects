rm(list = ls())

# load termDocMatrix
load("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 3/termDocMatrix.rdata")
termDocMatrix <- as.matrix(termDocMatrix)

# Transform Data into an Adjacency Matrix
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

termMatrix_undirected <- termMatrix
termMatrix_directed <- termMatrix * c(1,0,1,1,1)

source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 3/my_graph_measures.R")
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 3/my_graph_helper_functions.R")
termMatrix_undirected <- termMatrix_undirected
termMatrix_directed <- termMatrix_directed

# Test clustering coefficients
adjacency_matrix <- termMatrix_undirected

i <- 1
get_clustering_coefficient(adjacency_matrix, i)

clustering_coefficients <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  clustering_coefficients[i_idx] <- get_clustering_coefficient(adjacency_matrix, i_idx)
}
clustering_coefficients <- rbind(rownames(adjacency_matrix),clustering_coefficients)
print(clustering_coefficients)
print(get_average_clustering_coefficient(adjacency_matrix))


# Test centrality and prestige measures
adjacency_matrix <- termMatrix_undirected
i <- 1
print(get_degree_centrality(adjacency_matrix, i))

degree_centralities <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  degree_centralities[i_idx] <- get_degree_centrality(adjacency_matrix, i_idx)
}
degree_centralities <- rbind(rownames(adjacency_matrix),degree_centralities)
print(degree_centralities)

adjacency_matrix <- termMatrix_directed
i <- 1
print(get_degree_prestige(adjacency_matrix, i))

degree_prestiges <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  degree_prestiges[i_idx] <- get_degree_prestige(adjacency_matrix, i_idx)
}
degree_prestiges <- rbind(rownames(adjacency_matrix),degree_prestiges)
print(degree_prestiges)

i <- 1
print(get_gregariousness(adjacency_matrix, i))

degree_gregariousnesses <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  degree_gregariousnesses[i_idx] <- get_gregariousness(adjacency_matrix, i_idx)
}
degree_gregariousnesses <- rbind(rownames(adjacency_matrix),degree_gregariousnesses)
print(degree_gregariousnesses)


# Test closeness centrality and proximity prestige
adjacency_matrix <- termMatrix_undirected
i <- 1
print(get_closeness_centrality(adjacency_matrix, i))

closeness_centralities <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  closeness_centralities[i_idx] <- get_closeness_centrality(adjacency_matrix, i_idx)
}
closeness_centralities <- rbind(rownames(adjacency_matrix),closeness_centralities)
print(closeness_centralities)

adjacency_matrix <- termMatrix_directed
i <- 1
print(get_proximity_prestige(adjacency_matrix, i))

proximity_prestiges <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  proximity_prestiges[i_idx] <- get_proximity_prestige(adjacency_matrix, i_idx)
}
proximity_prestiges <- rbind(rownames(adjacency_matrix),proximity_prestiges)
print(proximity_prestiges)


# Test betweeness centrality
adjacency_matrix <- termMatrix_undirected[1:5,1:5]
i <- 1
print(get_betweenness_centrality(adjacency_matrix, i))

betweeness_centralities <- c()
for(i_idx in 1:nrow(adjacency_matrix)){
  betweeness_centralities[i_idx] <- get_betweenness_centrality(adjacency_matrix, i_idx)
}
betweeness_centralities <- rbind(rownames(adjacency_matrix),betweeness_centralities)
print(betweeness_centralities)


#Test link prediction measures
adjacency_matrix <- termMatrix_undirected
i <- 1
j <- 2
print(get_common_neighbor_based_measure(adjacency_matrix, i, j))
print(get_jaccard_measure(adjacency_matrix, i, j))


# Test community detection
source("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 3/my_graph_clustering.R")
term_matrix <- termMatrix_undirected[1:20,1:20]
res <- get_clusters_Kernighan_Lin(term_matrix)
print(res)


