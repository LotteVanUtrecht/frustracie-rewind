rants_berichten <- function(data,colors){
    rants <- data[["messages"]] %>% 
    select(c(time,author,text,id)) %>% 
    filter(grepl("rant:",text,ignore.case=TRUE)) %>% 
    mutate(rant=(!grepl("antirant:",text,ignore.case=TRUE)))
  
  rants_by_user <- rants %>% 
    group_by(author) %>% 
    summarize(n = length(text), rants = sum(rant)) %>% 
    mutate(antirants=n-rants) %>% 
    mutate(rant_frac=rants/n) %>% 
    mutate(rant_odds=log(rant_frac/(1-rant_frac))) %>% 
    mutate(messages=data[["messages"]]$author %>% summary())
  
  messages_per_rant <- nrow(data[["messages"]])/nrow(rants)
  
  
  plot <- rants_by_user %>% 
    ggplot(aes(x=messages,y=n,color=author)) +
    geom_point(show.legend = FALSE) +
    geom_text(aes(label=author),vjust=-1,hjust=0, show.legend = FALSE) +
    labs(title = "Frustracie Rewind 2022: Wie gebruikt de FrustraCie om te (anti)ranten?",x="Totaal berichten", y = "Totaal (anti)rants") +
    scale_x_log10(limits=c(120*messages_per_rant,1500*messages_per_rant),
                       labels = c(2000,5000,10000,15000),
                       minor_breaks = c(seq(2000,10000,1000),12500,15000),
                       breaks = c(2000,5000,10000,15000)) +
    scale_y_log10(limits=c(120,1500),
                       breaks = c(200,500,1000,1500), 
                       minor_breaks = c(seq(200,1000,100),1250,1500), 
                       labels = c(200,500,1000,1500)) +
    geom_line(mapping=aes(x=x,y=y),
              data.frame(x=c(120*messages_per_rant,1500*messages_per_rant),y=c(120,1500)),
              inherit.aes = FALSE) +
    coord_fixed(ratio=1) +
    scale_color_manual(values=colors)
  
  
  return(plot)
}

