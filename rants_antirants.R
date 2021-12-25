
rants_antirants <- function(data,colors){
  rants <- data[["messages"]] %>% 
    select(c(time,author,text,id)) %>% 
    filter(grepl("rant:",text,ignore.case=TRUE)) %>% 
    mutate(rant=(!grepl("antirant:",text,ignore.case=TRUE)))
  
  rants_by_user <- rants %>% 
    group_by(author) %>% 
    summarize(n = length(text), rants = sum(rant)) %>% 
    mutate(antirants=n-rants) %>% 
    mutate(rant_frac=rants/n) %>% 
    mutate(rant_odds=log(rant_frac/(1-rant_frac)))
  
  mean_rants <- sum(rants_by_user$rants)/sum(rants_by_user$n)
  
  rants_by_user$vjust <- -1
  rants_by_user$vjust[rants_by_user$author %in% c("Jeroen","Boris","Rosanne","Parcifal")] <- 1.25
  
  plot <- rants_by_user %>% 
    ggplot(aes(x=n,y=rant_frac,color=author)) +
    geom_point(show.legend = FALSE) +
    geom_text(aes(label=author,vjust=vjust),hjust=0, show.legend = FALSE) +
    labs(title = "Frustracie Rewind 2021: Rants & Antirants",x="Aantal (anti)rants", y = "Aandeel rants") +
    geom_hline(yintercept=0.5,linetype="dashed") +
    geom_text(aes(y=mean_rants,x=0,label="Gemiddelde"),hjust=0, vjust=-1,show.legend = FALSE,color="black",size=3) +
    geom_hline(yintercept=mean_rants,linetype="dotted") +
    scale_x_continuous(limits=c(16,1500),
                  labels = c(20,50,100,200,500,100,1000,1500),
                  minor_breaks = c(seq(20,90,by=10),seq(100,1000,by=100),1500),
                  breaks = c(20,50,100,200,500,100,1000,1500),trans="sqrt") +
    scale_y_continuous(limits=c(0.37,0.77),
                       breaks = seq(0.4,0.7,by=0.1), 
                       minor_breaks = seq(0.45,0.75,by=0.05), 
                       labels = c("40%","50%","60%","70%")) +
    scale_color_manual(values=colors)
  
  
  return(plot)
}
