
#maak categorieën
users <- data[["users"]] %>% 
  select(-total_words) %>% 
  mutate(
    Gender=ifelse(
    author %in% c("Boris","Cas","Jeroen","Niem","Orfeas","Reinout"),
    "Man","Vrouw") %>% as.factor(),
    Pangea=ifelse(
    author %in% c("Boris","Cas","Dagmar","Danee","Demi","Niem","Paco","Parcifal","Reinout","Rosanne","Sanne"),
    "Pangea","Niet-Pangea") %>% as.factor(),
    Burger=ifelse(
    author %in% c("Cas","Dagmar","Lotte","Maud","Nora","Parcifal","Reinout","Rosanne"),
    "Werkend","Studerend") %>% as.factor(),
    Relatiestatus=ifelse(
    author %in% c("Boris","Cas","Danee","Demi","Maud","Myrthe","Rosanne","Sanne"),
    "Relatie","Single") %>% as.factor())

#verzamel categorieën over dag
messages <- data[["messages"]] %>% 
  select(time,author) %>% 
  left_join(users) %>% 
  pivot_longer(c(Gender, Pangea, Burger, Relatiestatus),names_to = "categorie") %>% 
  mutate(categorie=as.factor(categorie),
         time=date(time)) %>% 
  group_by(time,categorie,value) %>% 
  summarise(n=n()) %>% 
  mutate(nweek=0)

#verzamel categorieën over week (NB: dit duurt verrekte lang. vermoed dat het iets te maken heeft met de tijdvergelijkingen)
for (i in 1:nrow(messages)){
  messages[i,"nweek"] <- messages %>% 
    filter(value==messages$value[i]) %>% 
    filter(time <= messages$"time"[i]) %>% 
    filter((messages$"time"[i]-weeks(1)) <= time) %>% 
    pull(n) %>% 
    sum()

}

messages <- messages %>% 
  group_by(time,categorie) %>% 
  summarise(nweek=nweek/sum(nweek),value=value) %>% 
  ungroup()

messages$categorie %>% relevel()

messages %>%
  ggplot(aes(x = time,y=nweek,fill=value)) +
  geom_area() +
  labs(title="Frustracie Rewind 2022: Balans in de FrustraCie",caption="Data tussen 1 en 10 januari ontbreekt en is geïntrapoleerd",x=NULL,y=NULL) +
  facet_wrap(~categorie,labeller = as_labeller(c("Burger"="Luie burgers of eeuwige studenten?",
                                                 "Gender"="Gedomineerd door vrouwen of mannen?",
                                                 "Pangea"="Pangeaparty of goede afspiegeling van AEGEE?",
                                                 "Relatiestatus"="Forever alone of gelukkig verliefd?"))) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal() +
  scale_fill_manual(values=c("Man"="lightblue",
                             "Vrouw"="#FF69B4",
                             "Pangea"="darkgreen",
                             "Niet-Pangea"="gold",
                             "Studerend"="Orange",
                             "Werkend"="Blue",
                             "Relatie"="Red",
                             "Single"="#39420F"))  +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=0.5))

