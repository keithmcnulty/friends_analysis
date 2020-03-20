library(rvest)
library(stringr)

scrape_friends <- function(season = 1, episode = 1) {
  
  url_string <- paste0("https://fangj.github.io/friends/season/", 
                       sprintf("%02d", season), 
                       sprintf("%02d", episode), 
                       ".html")
  
  nodes <- xml2::read_html(url_string) %>% 
    rvest::html_nodes("p") %>% 
    as.vector()
  
  node_text <- c()
  
  for (i in 1: length(nodes)) {
    if (grepl("Scene:", html_text(nodes[i]))) {
      node_text[i] <- "New Scene"
    } else if (length(rvest::html_nodes(nodes[i], "b")) == 0 & length(rvest::html_nodes(nodes[i], "strong")) == 0) {
      node_text[i] <- "Nothing"
    } else if (season == 10) {
      node_text[i] <- nodes[i] %>% 
        stringr::str_extract("(?<=<strong>).+?(?=</strong>:)")
    } else {
      node_text[i] <- nodes[i] %>% 
        stringr::str_extract("(?<=<b>).+?(?=:</b>)")
    }

      
  }
  
  node_text <- node_text[!grepl("/| and |All|Nothing", node_text)]
  node_text <- node_text[nchar(node_text) < 20]
  node_text <- gsub("<b>", "", node_text)
  node_text <- gsub("father", "dad", node_text)
  node_text <- gsub("mother", "mom", node_text)
  
  
  scene_count <- c()
  
  for (i in 1:length(node_text)) {
    scene_count[i] <- sum(grepl("New Scene", node_text[1:i])) + 1
  }
  
  data.frame(episode = episode, scene = scene_count, character = node_text) %>% 
    dplyr::filter(character != "New Scene") %>% 
    dplyr::distinct(episode, scene, character)
  
  
}
