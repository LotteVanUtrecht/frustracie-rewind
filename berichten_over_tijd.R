create_time_summary <- function(data,colors=colors){
  
  time_messages <- data[["messages"]] %>% 
    select(-c(emoji,emoji_name)) %>% 
    mutate(month=floor_date(time, unit="3 months"),day=day(time),hour=hour(time))
  
  #assumes at least one message every day
  
  total_messages <- time_messages %>% group_by(author) %>% count(name="total_n")
  
  messages_per_month <- time_messages %>% 
    group_by(author,month) %>% 
    summarise(n=n()) %>% 
    left_join(total_messages) %>% 
    mutate(n=(n/total_n))
  
  plot_months_1 <- messages_per_month %>%
    filter(month>date("2017-12-31")) %>% 
    ggplot(aes(x=month,y=n,fill=author)) +
    geom_col(show.legend = FALSE)  +
    theme_classic() +
    scale_fill_manual(values=colors) +
    scale_y_continuous(breaks=NULL,labels=NULL) +
    scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
    facet_wrap(vars(author)) +
    labs(title = "Boni Zoomers: Activiteit over de afgelopen vier jaar",x=NULL,y = NULL) 

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
 
 plot_hours <- messages_per_hour %>% 
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
    labs(title = "Boni Zoomers: wie is wanneer actief?",x=NULL,y = NULL)
  
  return(list(plot_months_1,plot_hours))
  
  }
