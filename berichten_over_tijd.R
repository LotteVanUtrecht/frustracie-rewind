create_time_summary <- function(data,colors=colors){
  
  time_messages <- data[["messages"]] %>% 
    select(-c(emoji,emoji_name)) %>% 
    mutate(month=month(time),day=day(time),hour=hour(time))
  
  days_per_month <- time_messages %>% group_by(day,month) %>% summarise() %>% group_by(month) %>% summarise(ndays=n())
  #assumes at least one message every day
  
  total_messages <- time_messages %>% group_by(author) %>% count(name="total_n")
  
  messages_per_month <- time_messages %>% 
    group_by(author,month) %>% 
    summarise(n=n()) %>% 
    left_join(days_per_month) %>% 
    mutate(normalized_n = (n/ndays)*(346/12)) %>% 
    left_join(total_messages) %>% 
    mutate(n=(n/total_n))
  
  sorting_order_month <- messages_per_month %>% 
    group_by(author) %>% 
    summarize(vroeg_laat=sum(month*n)) %>% 
    arrange(vroeg_laat) %>% 
    pull(author)
  
  for (name in rev(sorting_order_month)){ #zorgt ervoor dat de plots op de volgorde komen die ik wil
    messages_per_month$author <- relevel(messages_per_month$author,name)
    }

  plot_months_1 <- messages_per_month %>% 
    filter((author %in% sorting_order_month[1:16])) %>% 
    ggplot(aes(x=month,y=n,fill=author)) +
    geom_col(show.legend = FALSE)  +
    theme_classic() +
    scale_fill_manual(values=colors) +
    scale_y_continuous(breaks=NULL,labels=NULL) +
    scale_x_continuous(breaks=1:12,
                     labels=c("jan","feb","mar","apr","mei","jun","jul","aug","sep","okt","nov","dec")) +
    facet_wrap(vars(author)) +
    labs(title = "Frustracie Rewind 2021: Wie was actief in welke maanden?",x=NULL,y = NULL) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  plot_months_2 <- messages_per_month %>% 
    filter(!(author %in% sorting_order_month[1:16])) %>% 
    ggplot(aes(x=month,y=n,fill=author)) +
    geom_col(show.legend = FALSE)  +
    theme_classic() +
    scale_fill_manual(values=colors) +
    scale_y_continuous(breaks=NULL,labels=NULL) +
    scale_x_continuous(breaks=1:12,
                       labels=c("jan","feb","mar","apr","mei","jun","jul","aug","sep","okt","nov","dec")) +
    facet_wrap(vars(author)) +
    labs(title = "Frustracie Rewind 2021: Wie was actief in welke maanden?",x=NULL,y = NULL) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  messages_per_hour <- time_messages %>% 
    group_by(author,hour) %>% 
    summarise(n=n()) %>% 
    left_join(total_messages) %>% 
    mutate(n=(n/total_n))
    
  sorting_order_hour <- messages_per_hour %>% 
    group_by(author) %>% 
    summarize(vroeg_laat=sum(((hour-6)%%24)*n)) %>% 
    arrange(vroeg_laat) %>% 
    pull(author)
  
  for (name in rev(sorting_order_hour)){ #zorgt ervoor dat de plots op de volgorde komen die ik wil
    messages_per_hour$author <- relevel(messages_per_hour$author,name)
  }
  
  plot_hours <- messages_per_hour %>% 
    filter(author!="Nora") %>% 
    ggplot(aes(x=hour,y=n,fill=author)) +
    geom_col(show.legend = FALSE)  +
    theme_classic() +
    scale_fill_manual(values=colors) +
    scale_y_continuous(breaks=NULL,labels=NULL) +
    geom_vline(xintercept=-0.5,linetype="dotted") + 
    geom_vline(xintercept=5.5,linetype="dotted") + 
    geom_vline(xintercept=11.5,linetype="dotted") + 
    geom_vline(xintercept=17.5,linetype="dotted") +
    geom_vline(xintercept=23.5,linetype="dotted") +
    scale_x_continuous(breaks=seq(2.5,20.5,6),
                       labels=c("Nacht","Ochtend","Middag","Avond")) +
    facet_wrap(vars(author),ncol=3) +
    labs(title = "Frustracie Rewind 2021: Wie spamt wanneer?",x=NULL,y = NULL)
  
  return(list(plot_months_1,plot_months_2,plot_hours))
  
  }
