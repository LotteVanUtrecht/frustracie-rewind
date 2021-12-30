visualize_wordcloud <- function(tf_icf){
  
  authors <- levels(tf_icf$author)
  
  wordclouds <- list()

  for (person in authors){
    
    if (person %in% c("Jesse","Nora","Bente","Orfeas","Sanne")){
    
      wordclouds[[person]] <- tf_icf %>% 
          filter((author==person)&n>2&tf_icf>2) %>%
          select(word,tf_icf) %>% 
          mutate(tf_icf=log(tf_icf)) %>% 
          head(100) %>% 
          wordcloud2(color="random-light",size=.60,minSize="FrustraCie rewind: Iemands meest typische FrustraCie woorden")
    }
    else {
      wordclouds[[person]] <- tf_icf %>% 
        filter(author==person&n>3&tf_icf>2) %>%
        select(word,tf_icf) %>% 
        mutate(tf_icf=log(tf_icf)) %>% 
        head(100) %>% 
        wordcloud2(color="random-dark",size=.60)
    }
      
  }
  
  return(wordclouds)
}

wordclouds <- visualize_wordcloud(tf_icf)

random_order <- sample(authors)

wordclouds[random_order]
