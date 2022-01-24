visualize_wordcloud <- function(tf_icf){
  
  authors <- levels(tf_icf$author)
  
  wordclouds <- list()

  for (person in authors){
      wordclouds[[person]] <- tf_icf %>% 
        filter(author==person&n>3&tf_icf>2) %>% 
        filter(!pmap(list(author,word,T),grepl) %>% unlist()) %>%
        select(word,tf_icf) %>% 
        mutate(tf_icf=log(tf_icf)) %>% 
        head(100) %>% 
        wordcloud2(color="random-dark",size=.25,shape="star",minSize = "Jouw tekst hier?")
 
  }
  
  return(wordclouds)
}

tf_icf <- compute_tf_icf(data[["words"]],data[["users"]])
wordclouds <- visualize_wordcloud(tf_icf)

random_order <- sample(authors)
