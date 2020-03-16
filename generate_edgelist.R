raw_results <- data.frame(from = character(), to = character())

for (season in 1:10) {
  for (episode in 1:25) {
    print(paste("Scraping Season", season, "Episode", episode))
    scrape <- tryCatch(
      scrape_friends(season, episode),
      error = function(e) {data.frame(from = character(), to = character())}
    )
    
    result <- data.frame(from = character(), to = character())
    
    if (nrow(scrape) > 0) {
      for (i in 1:max(scrape$scene)) {
        result_new <- scrape %>% 
          dplyr::filter(scene == i) %>% 
          dplyr::pull(character) %>% 
          unique_pairs()
        
        result <- result %>% 
          dplyr::bind_rows(result_new) 
      } 
    } else {
      result <- data.frame(from = character(), to = character())
    }
    
   raw_results <- raw_results %>% 
     dplyr::bind_rows(result)
     
  }
  
  raw_results <- raw_results %>% 
    dplyr::mutate(season = season)
}


for (i in 1: nrow(raw_results)) {
  
  raw_results[i, c("from", "to")] <- sort(raw_results[i, c("from", "to")])
  
}

edges <- raw_results %>% 
  dplyr::count(season, from, to, name = "weight")

if (!dir.exists("data")) {
  dir.create("data")
}

write.csv(edges, "data/friends_edgelist.csv", row.names = FALSE)
