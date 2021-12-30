compute_tf_icf <- function(words, users){
  ###Input:
  #Words (tbl): Data containing the # of occurences of each author-word combination, as outputted by preprocess_chatlog()
  #Users (tbl): Data containing the # of words for each user, as outputted by preprocess_chatlog(). Could be removed.
  ###Output
  #tf_icf: A tibble identical to words, but with the tf-icf added
  
  
  word_freqs <- words %>% 
    group_by(word) %>% 
    summarize(n_word = sum(n))
  
  n_total <- sum(users$total_words)
  
  tf_icf <- words %>% 
    left_join(word_freqs) %>% 
    left_join(users) %>% 
    rename(n_author = total_words) %>% 
    mutate(tf=n/n_author) %>% 
    mutate(tf_complement=(n_word-n+1)/(n_total-n_author+1)) %>% 
    mutate(tf_icf=tf/tf_complement) %>% #alternatively, take the log, because the bar plots will look nicer
    select(c(author,word,n,tf_icf)) %>% 
    arrange(desc(tf_icf))

  return(tf_icf)
}
