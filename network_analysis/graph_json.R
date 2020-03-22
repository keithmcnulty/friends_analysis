library(igraph)
library(tidyverse)
library(networkD3)
library(jsonlite)

graph_json <- function(season = NULL, edgelist = NULL) {
  
  edgelist_matrix <- as.matrix(edgelist[ ,c("from", "to")])
  
  # create igraph network
  graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
    igraph::set.edge.attribute("weight", value = edgelist$weight)
  
  # calculate degree centrality for each node
  V(graph)$degree <- igraph::degree(graph)
  
  # find louvain communities
  louvain_partition <- igraph::cluster_louvain(graph, weights = E(graph)$weight)
  
  #assign communities to graph
  graph$community <- louvain_partition$membership
  
  # convert got_graph for D3 - this creates a list of two dataframes - one for nodes and one for edges
  
  d3_network <- networkD3::igraph_to_networkD3(graph, group = graph$community)
  
  # for community coloring in our D3 graph we want our edges to be coded according to their starting node
  
  d3_network$nodes <- d3_network$nodes %>% 
    tibble::rownames_to_column()
  d3_network$links <- dplyr::left_join(d3_network$links, 
                                       d3_network$nodes %>% 
                                         dplyr::mutate(rowname = as.numeric(rowname) - 1) %>% 
                                         dplyr::select(rowname, group),
                                       by = c("source" = "rowname")) 
  d3_network$nodes <- d3_network$nodes %>% 
    dplyr::select(-rowname)
  
  # capture the degree centrality of the nodes
  d3_network$nodes$degree <- V(graph)$degree
  
  # capture the weights of the edges
  d3_network$links$weight <- E(graph)$weight
  
  jsonlite::write_json(d3_network, paste0("data/friends_network_s", season, ".json"))
}