####Import packages
library("rwhatsapp")
library("tidyverse")
library("tidytext")
library("scales")
library("RColorBrewer")
library("lubridate")


####Import data
source("1_data_import.R")


data <- preprocess_chatlog("Data/combined_2021.txt",min_words=700)


####Set colors

colors <- c(brewer.pal(9,"Set1")[1:5],
            "#F8B500",brewer.pal(9,"Set1")[7:9],brewer.pal(7,"Set2")[1],
            "#800000","#000000","turquoise","#cc99cc","yellow4",
            brewer.pal(7,"Set3")[3:7],
            "#1129BF","#40826D")

names(colors)=c(
  "Boris","Danee","Parcifal","Paco","Lotte",
  "Myrthe","Nora","Annette","Cas","Rosanne",
  "Dagmar","Niem","Elke","Demi","Reinout",
  "Jeroen","Maud","Orfeas","Bente","Jesse",
  "Sanne","Jippe"
)

####Set colors

#nb: actiefste en minst actieve dagen