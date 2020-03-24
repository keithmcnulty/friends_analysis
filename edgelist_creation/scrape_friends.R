library(rvest)
library(stringr)

#' Generate a data frame of distinct characters by scene for an episode of Friends.  
#' Non existing episodes, including the second part of two parters, will return an error.
#'
#' @param season Season number, integer
#' @param episode Episode number integer
#' @return A dataframe with three columns - episode, scene, character

scrape_friends <- function(season = 1, episode = 1) {
  
  # some episodes are double episodes
  
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
  
  # general html for seasons 1:9 is different from season 10
  
  if (season %in% 1:9) {
    nodes <- xml2::read_html(url_string) %>% 
      xml2::as_list() %>% 
      unlist()
    
    nodes <- ifelse(grepl("Scene:", nodes), "New Scene", nodes) # mark scene boundaries
    
    # season 2 has some weirdly formatted episodes
    if (season == 2 & episode %in% 3:25) {
      nodes <- ifelse(nodes != "New Scene",  
                      stringr::str_extract(nodes, "[[[:upper:]][[:punct:]]\\s]+(?=:)") %>% # look for caps preceeding a colon
                        tolower() %>% 
                        tools::toTitleCase(),
                      nodes)
    } else {
      nodes <- ifelse(nodes != "New Scene", 
                      stringr::str_extract(nodes, ".+?(?=:)"), #look for anything precedeing a colon
                      nodes)
    }
    
  } else {
    # season 10
    nodes <- xml2::read_html(url_string) %>% 
      rvest::html_nodes("p") %>% 
      rvest::html_text() # anything in paragraph tags
    
    nodes <- ifelse(grepl("Scene:", nodes), "New Scene", nodes)
    
    nodes <- ifelse(nodes != "New Scene", 
                    stringr::str_extract(nodes, ".+?(?=:)"), # anything preceding a colon
                    nodes)
    
  }
  

  # manual leaveouts and replacements - gets us 98% of the way I reckon 
  nodes <- nodes[!is.na(nodes)]
  nodes <- trimws(nodes)
  nodes <- nodes[!grepl("/| and |all|everybody|&|by|position|aired|both|,|from|at 8|end|time|commercial|\\(|\\[|letters| to |it's|it was|kudrow|perry|cox|aniston|schwimmer|leblanc|look|could|walks|everyone|teleplay|story|together", tolower(nodes))]
  nodes <- nodes[nchar(nodes) < 20]
  nodes <- nodes[!grepl("^[a-z]|^[0-9]|^[[:punct:]]", nodes)]
  nodes <- gsub("<b>|\n", "", nodes)
  nodes <- gsub("\u0092", "'", nodes)
  nodes <- gsub("Mr.Heckles", "Mr. Heckles", nodes)
  nodes <- gsub("father", "dad", nodes)
  nodes <- gsub("mother", "mom", nodes)
  nodes <- gsub("Mr. ", "Mr ", nodes)
  nodes <- gsub("Mrs. ", "Mrs ", nodes)
  nodes <- gsub("Ms. ", "Ms ", nodes)
  nodes <- gsub("Dr. ", "Dr ", nodes)
  nodes <- ifelse(nodes == "r Zelner", "Mr Zelner", nodes)
  nodes <- ifelse(nodes == "r Zelner", "Mr Zelner", nodes)
  nodes <- ifelse(nodes == "Mnca", "Monica", nodes)
  nodes <- ifelse(nodes == "Phoe", "Phoebe", nodes)
  nodes <- ifelse(nodes == "Rach", "Rachel", nodes)
  nodes <- ifelse(nodes == "Chan", "Chandler", nodes)
  nodes <- ifelse(nodes == "Billy", "Billy Crystal", nodes)
  nodes <- ifelse(nodes == "Robin", "Robin Williams", nodes)
  nodes <- ifelse(tolower(nodes) == "amger", "amber", nodes)
  
  # number each scene
  scene_count <- c()
  
  for (i in 1:length(nodes)) {
    scene_count[i] <- sum(grepl("New Scene", nodes[1:i])) + 1
  }
  
  data.frame(episode = episode, scene = scene_count, character = nodes) %>% 
    dplyr::filter(character != "New Scene") %>% 
    dplyr::distinct(episode, scene, character) %>% 
    dplyr::mutate(scene = scene - min(scene) + 1, # set first scene number to 1
                  character = character %>% tolower() %>% tools::toTitleCase()) # takes care of caps mistyping
  
  
}
