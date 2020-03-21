library(rvest)
library(stringr)

#' Generate a data frame of distinct characters by scene for an episode of Friends.  
#' Non existing episodes, including the second part of two parters, will return an error.
#'
#' @param season Season number, integer
#' @param episode Episode number integer
#' @return A dataframe with three columns - episode, scene, character

scrape_friends <- function(season = 1, episode = 1) {
  
  if (season == 2 & episode == 12) {
    url_string <- "https://fangj.github.io/friends/season/0212-0213.html"
  } else if (season == 6 & episode == 15) {
    url_string <- "https://fangj.github.io/friends/season/0615-0616.html"
  } else if (season == 9 & episode == 23) {
    url_string <- "https://fangj.github.io/friends/season/0923-0924.html"
  } else if (season == 10 & episode == 17) {
    url_string <- "https://fangj.github.io/friends/season/1017-1018.html"
  } else {
    url_string <- paste0("https://fangj.github.io/friends/season/", 
                         sprintf("%02d", season), 
                         sprintf("%02d", episode), 
                         ".html")
  }
  
  nodes <- xml2::read_html(url_string) %>% 
    rvest::html_nodes("p") %>% 
    xml2::as_list() %>% 
    unlist() %>% 
    as.vector()
  
  nodes <- ifelse(grepl("Scene:", nodes), "New Scene", nodes)

  if (season == 10) {
    nodes <- ifelse(nodes != "New Scene", 
                    stringr::str_extract(nodes, ".+?(?=:)"),
                    nodes)
  } else if (season == 2 & episode %in% 3:25) {
    nodes <- ifelse(nodes != "New Scene",  
                    stringr::str_extract(nodes, "[[[:upper:]][[:punct:]]\\s]+(?=:)") %>% 
                      tolower() %>% 
                      tools::toTitleCase(),
                    nodes)
  } else {
    nodes <- ifelse(nodes != "New Scene", 
                    stringr::str_extract(nodes, ".+?(?=:)"),
                    nodes)
  }


  
  nodes <- nodes[!grepl("/| and |All|Nothing|&", nodes)]
  nodes <- nodes[nchar(nodes) < 20]
  nodes <- gsub("<b>|\n", "", nodes)
  nodes <- gsub("father", "dad", nodes)
  nodes <- gsub("mother", "mom", nodes)
  nodes <- ifelse(nodes == "Phoe", "Phoebe", nodes)
  nodes <- ifelse(nodes == "Rach", "Rachel", nodes)
  nodes <- ifelse(nodes == "Chan", "Chandler", nodes)
  
  scene_count <- c()
  
  for (i in 1:length(nodes)) {
    scene_count[i] <- sum(grepl("New Scene", nodes[1:i])) + 1
  }
  
  data.frame(episode = episode, scene = scene_count, character = nodes) %>% 
    dplyr::filter(character != "New Scene") %>% 
    dplyr::distinct(episode, scene, character) %>% 
    dplyr::mutate(scene = scene - min(scene) + 1)
  
  
}
