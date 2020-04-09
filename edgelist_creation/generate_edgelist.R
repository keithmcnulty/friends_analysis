library(dplyr)
source('edgelist_creation/scrape_friends.R')
source('edgelist_creation/unique_pairs.R')

# iterate through each episodes - pass no data if an error occurs (ie episode doesn't exist)

raw_results <- data.frame(season = integer(), from = character(), to = character(), stringsAsFactors = FALSE)

for (season in 1:10) {
  
  season_results <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  
  # no season has more than 25 episodes
  for (episode in 1:25) { 
    
    message(paste("Scraping Season", season, "Episode", episode))
    
    # pass empty df if scrape fails
    scrape <- tryCatch(
      scrape_friends(season, episode),
      error = function(e) {
        message(paste("Scrape failed for Episode", episode, "Season", season))
        data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
        }
    )
    
    result <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
    
    if (nrow(scrape) > 0) {
      result <- scrape %>% 
        dplyr::group_by(scene) %>% 
        dplyr::summarise(unique_pairs(character)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-scene)
    } 
    
   season_results <- season_results %>% 
     dplyr::bind_rows(result)
     
  }
  
  raw_results <- season_results %>% 
    dplyr::mutate(season = season) %>% 
    dplyr::bind_rows(raw_results)
  
}


# order pairs alphabetically to deal with different orderings
for (i in 1: nrow(raw_results)) {
  
  raw_results[i, c("from", "to")] <- sort(raw_results[i, c("from", "to")])
  
}

# add up scenes to form season weights
edges <- raw_results %>% 
  dplyr::count(season, from, to, name = "weight")

if (!dir.exists("data")) {
  dir.create("data")
}

saveRDS(edges, "data/friends_edgelist_by_season.RDS")
write.csv(edges, "data/friends_edgelist_by_season.csv", row.names = FALSE)

# add up over the entire series
edges <- as.data.frame(edges)

for (i in 1:nrow(edges)) {
  
  edges[i, c("from", "to")] <- sort(edges[i, c("from", "to")])
  
}

full_series <- edges %>% 
  dplyr::group_by(from, to) %>% 
  dplyr::summarise(weight = sum(weight))

saveRDS(full_series, "data/friends_full_series_edgelist.RDS")
write.csv(full_series, "data/friends_full_series_edgelist.csv", row.names = FALSE)
