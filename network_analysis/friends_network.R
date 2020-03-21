library(tidyverse)
library(readr)
library(igraph)

friends_network <- function(season = NULL, edgelist = NULL, type = c("mds", "sphere")) {
  
  friends <- c("Phoebe", "Monica", "Rachel", "Joey", "Ross", "Chandler")

  edgelist_matrix <- as.matrix(edgelist[ ,c("from", "to")])
  
  graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
    igraph::set.edge.attribute("weight", value = edgelist$weight)

  
  # run louvain with edge weights
  
  louvain_partition <- igraph::cluster_louvain(graph, weights = E(graph)$weight)
  
  graph$community <- louvain_partition$membership
  
  # find highest betweenness centrality nodes
  
  high_btwn_nodes <- c()
  
  for (i in 1:length(unique(graph$community))) {
    subgraph <- induced_subgraph(graph, v = which(graph$community == i))
    btwn <-  igraph::betweenness(subgraph)
    high_btwn_nodes[i] <- names(which(btwn == max(btwn)))
  }
  
  # give our nodes some properties, incl scaling them by degree and coloring them by community
  
  V(graph)$size <- 5
  V(graph)$frame.color <- "white"
  V(graph)$color <- graph$community
  V(graph)$label <- V(graph)$name
  V(graph)$label.cex <- 3
  
  # also solor edges according to their starting node
  edge.start <- ends(graph, es = E(graph), names = F)[,1]
  E(graph)$color <- V(graph)$color[edge.start]
  E(graph)$arrow.mode <- 0
  
  # only label central characters
  
  v_labels <- which(V(graph)$name %in% friends)
  
  for (i in 1:length(V(graph))) {
    if (!(i %in% v_labels)) {
      V(graph)$label[i] <- ""
    }
  }
  
  if (type == "sphere") {
    l1 <- layout_on_sphere(graph)
    plot(graph, rescale = F, layout = l1, main = paste("'Friends' Network up to Season", season))
  } else {
    l2 <- layout_with_mds(graph)
    p2 <- plot(graph, rescale = F, layout = l2*0.5, main = paste("'Friends' Network up to Season", season))
  }
  
}