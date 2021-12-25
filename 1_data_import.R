####Import data

preprocess_chatlog <- function(chatlog,min_words=1000,first_names_only=TRUE){
  ###In:
  #chatlog (tbl or chr): Any WhatsApp chatlog, as outputted from rwa_read(), or any path to a chatlog
  #min_words:
  #first_names_only (bool): If true (default), removes all characters in user names after the first space
  
  ###Out:
  #data (list), containing:
  ##users (tbl): contains names of users to create graphics for and their total number of words
  ##messages (tbl): contains all messages with their time, text, author and emoji
  ##words (tbl): contains all word/author combinations with their number of occurences
  
  if(class(chatlog)=="character"){chatlog <- rwa_read(chatlog)}
  
  messages <- chatlog %>% 
    select(!"source") %>% 
    filter(!(text %in% c("<Media omitted>","You deleted this message","This message was deleted"))) %>% 
    filter(!author %in% c("Anne M AEGEE","Dominique AEGEE","Floor AEGEE","Tess AEGEE")) %>% 
    droplevels() %>% 
    drop_na(author) #removes messages like "you are now an admin of this group"
  
  if(first_names_only==TRUE){levels(messages$author) <- str_remove_all(levels(messages$author),"[:blank:](\\w)*")}
  
  words <- messages %>% 
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE)
  
  users <- words %>%
    group_by(author) %>% 
    summarize(total_words = sum(n)) %>% 
    filter(total_words >= min_words) %>% 
    droplevels()
  
  messages <- messages %>% 
    filter(author %in% users$author) %>% 
    droplevels()
  words <- words %>% 
    filter(author %in% users$author) %>% 
    droplevels()
  
  data <-  list(users=users,messages=messages,words=words)
  return(data)
}
