visualize_wordcloud_rants <- function(data){
  rants <- data[["messages"]] %>% 
    select(c(time,author,text,id)) %>% 
    filter(grepl("rant:",text,ignore.case=TRUE)) %>% 
    mutate(rant=(!grepl("antirant:",text,ignore.case=TRUE)))
  
  rants_by_user <- rants %>% 
    group_by(author) %>% 
    summarize(n = length(text), rants = sum(rant)) %>% 
    mutate(antirants=n-rants) %>% 
    mutate(rant_frac=rants/n) %>% 
    column_to_rownames("author")
    
  messages <- data[["messages"]] %>%
    left_join(rants %>% select(id,rant)) %>% 
    mutate(rant=rant %>% 
             as.character() %>% 
             replace_na("None") %>% 
             as.factor() %>% 
             recode_factor("TRUE"="Rant","FALSE"="Antirant","None"="None")) %>% 
    select(-c(time,emoji,emoji_name))
  
  words <- messages %>% 
    unnest_tokens(word, text) %>%
    count(author, rant, word, sort = TRUE)
  
  n_categories <- words %>%
    group_by(author,rant) %>% 
    summarize(total_words = sum(n)) %>% 
    droplevels()
  
  visualization <- list()
  
  for (person in data[["users"]]$author){
    
    tf_icf <- words %>% 
      filter(author==person) %>% 
      left_join(data[["words"]] %>% filter(author==person) %>% rename(n_word=n)) %>% 
      left_join(n_categories) %>% 
      rename(n_cat = total_words) %>% 
      mutate(tf=n/n_cat) %>% 
      mutate(tf_complement=(n_word-n+2)/(n_total-n_cat+2)) %>% 
      mutate(tf_icf=tf/tf_complement) %>% #take the log, because the bar plots will look nicer
      select(c(rant,word,n,tf_icf)) %>% 
      arrange(desc(tf_icf)) %>% 
      filter(!(word %in% c("rant","antirant","anti"))) %>% 
      filter(n>1&tf_icf>1.5)
    
    words_in_wordcloud <- 60
    
    tf_icf_selection <- bind_rows(
      (tf_icf %>% 
        filter(rant=="Rant") %>% 
        head(round(rants_by_user[person,"rant_frac"]*words_in_wordcloud)) %>% 
        mutate(tf_icf=10*tf_icf/mean(tf_icf))),
      (tf_icf %>% 
         filter(rant=="Antirant") %>% 
         head(round((1-rants_by_user[person,"rant_frac"])*words_in_wordcloud)) %>% 
         mutate(tf_icf=10*tf_icf/mean(tf_icf)))
       ) %>% 
      arrange(desc(tf_icf))
    
    visualization[[person]] <- tf_icf_selection %>%
      select(word,tf_icf) %>% 
      wordcloud2(shape="star",
                 size=0.4,
                 color = ifelse(tf_icf_selection$rant == "Rant", 'red', 'darkgreen'))
    
  }
  
  return(visualization)
}

stars <- visualize_wordcloud_rants(data)
random_order <- sample(data[["users"]]$author)
