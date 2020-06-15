friends_episode <- data.frame(
  scene = c(1, 1, 1, 2, 2, 2),
  character = c("Joey", "Phoebe", "Chandler", "Joey", "Chandler", "Janice")
)



unique_pairs <- function(char_vector = NULL) {
  
  vector <- as.character(unique(char_vector))
  
  df <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  
  if (length(vector) > 1) {
    for (i in 1:(length(vector) - 1)) {
      from <- rep(vector[i], length(vector) - i) 
      to <- vector[(i + 1): length(vector)]
      
      df <- df %>% 
        dplyr::bind_rows(
          data.frame(from = from, to = to, stringsAsFactors = FALSE) 
        )
    }
  }
  
  df
  
}



model_coefs <- function(formula, data) {
  coefs <- lm(formula, data)$coefficients
  data.frame(coef = names(coefs), value = coefs)
}
