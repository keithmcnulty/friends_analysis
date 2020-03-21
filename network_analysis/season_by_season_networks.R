library(tidyverse)
library(jpeg)
source("network_analysis/friends_network.R")

# get friends full series edgelist

edgefile_url <- "https://github.com/keithmcnulty/friends_analysis/blob/master/data/friends_edgelist_by_season.RDS?raw=true"
download.file(edgefile_url, "edgelist.RDS")

edgelist <- readRDS("edgelist.RDS")

# before we do anything lets break the connections between the six friends, so they don't dominate our community analysis

friends <- c("Phoebe", "Monica", "Rachel", "Joey", "Ross", "Chandler")
edgelist_without <- edgelist %>% 
  dplyr::filter(!(from %in% friends & to %in% friends))

# now generate ten season dfs

for (i in 1:10) {
  df <- edgelist_without %>% 
           dplyr::filter(season <= i) %>% 
           dplyr::select(-season) %>% 
           as.data.frame()
  
  for (j in 1:nrow(df)) {
    
    df[j, c("from", "to")] <- sort(df[j, c("from", "to")])
    
  }
  
  assign(paste0("edgelist_s", i),
         df %>% 
           dplyr::group_by(from, to) %>% 
           dplyr::summarise(weight = sum(weight)))
  
  
}

if (!dir.exists("img")) {
  dir.create("img")
}

for (i in 1:10) {
  
  jpeg(paste0("img/friends_s", i, "_sphere.jpeg"), width = 1200, height = 1200)
  friends_network(i, get(paste0("edgelist_s", i)), type = "sphere")
  dev.off()
  
}


