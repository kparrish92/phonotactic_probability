##########################################
# Helper functions to help calculate phonotactic probability given a corpus (containing a word and token frequency)
# and a word or list of words or non-words
##########################################
# segment word 

segment <- function(word) {
  word_seg_df <- data.frame(value = str_split(word, pattern = "")[[1]]) %>% 
    mutate(segment = c(1:nchar(word))) %>% 
    mutate(word = word)
  return(word_seg_df)
}



##########################################
# segment all words in a df 

segment_df <- function(corpus) {
  output <- character()
  for(thisRun in 1:nrow(corpus)){
    df <- segment(word = corpus$word[thisRun])
    output <- rbind(df, output)
  }
  return(output)
}

##########################################
bigram <- function(word)
{
  word_i = segment(word)
  k <- nrow(word_i) - 1
  bigram_df <- matrix(nrow = k, ncol = 3)
  colnames(bigram_df) <- c("bigram", "bigram_position", "word")
  for(iteration in 1:k)
  {
    segment1 = word_i$value[iteration] 
    segment2 = word_i$value[iteration + 1] 
    bigram_df[iteration, 1] = str_c(segment1, segment2)
    bigram_df[iteration, 2] = iteration
    bigram_df[iteration, 3] = word
    
  }
  return(bigram_df)
}


##########################################

# function given tidy corpus with log_freq vector that has been sliced to bigrams, and a word
# can use non-english words on the english corpus 

find_bigram_prob <- function(word, bigram_df, corpus)
{
  
  word_df <- bigram(word) %>% 
    as.data.frame()
  steps <- matrix(nrow = nrow(word_df))
  for (thisRun in 1:nrow(word_df)) {
    num_df <- bigram_df %>% 
      filter(bigram == word_df$bigram[thisRun] & bigram_position == word_df$bigram_position[thisRun])
    den_df <- corpus %>% 
      filter(nchar(word) > thisRun)
    
    num <- sum(num_df$log_freq)  
    den <- sum(den_df$log_freq)
    
    steps[thisRun] <- num/den
  }
  prob = sum(steps)/nrow(word_df)
  return(prob)
}




