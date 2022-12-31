data_last_year <- preprocess_chatlog("Data/2021/combined_2021.txt",min_words=10000)

data_total <- data[["words"]] %>% 
  add_column(year=2022,
             n_year=data[["words"]]["n"] %>% sum()) %>% 
  rbind(
    data_last_year[["words"]] %>% 
          add_column(year=2021,
                     n_year=data_last_year[["words"]]["n"] %>% sum()),
        by=character()) %>% 
  group_by(word,year,n_year) %>% 
  summarise(n=sum(n)) %>% 
  mutate(tf=n/n_year) %>% 
  pivot_wider(id_cols=word,names_from = year,values_from = c(n,n_year)) %>% 
  replace_na(list(word="",n_2022=0,n_2021=0,n_year_2022=data[["words"]]["n"] %>% sum(),n_year_2021=data_last_year[["words"]]["n"] %>% sum()))

c <- 1

new_words <- data_total %>% 
  mutate(tf_2022=(n_2022+2)/n_year_2022,
         tf_2021=(n_2021+2)/n_year_2021,
         tf_ratio=tf_2022/tf_2021)
