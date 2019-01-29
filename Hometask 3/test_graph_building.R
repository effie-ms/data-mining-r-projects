rm(list = ls())

# load termDocMatrix
load("D:/Effie/TTU/Data Mining and Network Analysis/Practices/Hometask 3/termDocMatrix.rdata")
termDocMatrix <- as.matrix(termDocMatrix)

# Transform Data into an adjacency Matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

termMatrix_undirected <- termMatrix
termMatrix_directed <- termMatrix * c(0,0,1)

build_graph <- function(adjacency_matrix, is_directed_graph){
  library(igraph)
  graph_mode <- "undirected"
  if(is_directed_graph == TRUE){
    graph_mode <- "directed"
  }
  # build a graph from the above matrix
  g <- graph.adjacency(adjacency_matrix, weighted=T, mode = graph_mode)
  # remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
  V(g)$label.color <- rgb(0, 0, .2, .8)
  V(g)$frame.color <- NA
  egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
  E(g)$color <- rgb(.5, .5, 0, egam)
  E(g)$width <- egam
  tkplot(g, layout=layout.kamada.kawai)
}

build_graph(termMatrix_undirected[1:20,1:20], FALSE)
build_graph(termMatrix_directed, TRUE)
