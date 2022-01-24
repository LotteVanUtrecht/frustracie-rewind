####Import packages
library("rwhatsapp")
library("tidyverse")
library("tidytext")
library("scales")
library("RColorBrewer")
library("lubridate")
library("wordcloud2")


####Import data
source("1_data_import.R")


data <- preprocess_chatlog("Data/derdeklassertjes.txt")


####Set colors

colors <- c(brewer.pal(9,"Set1")[-6],"#800000","#000000","#F8B500")

names(colors)=levels(data[["users"]][["author"]])

####Set colors

