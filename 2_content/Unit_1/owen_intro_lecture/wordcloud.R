library(tidyverse)
library(janitor)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

words <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vREbywoQ3q6U3ibxnnLL1gDG89cynoGeT6Gdr5Ye40PGaxG-0jlSmMaoEsoVj2jPUaZA3Cd8mQns1LL/pub?gid=459099465&single=true&output=csv") %>%
  clean_names() %>%
  rename(word = see_above) %>%
  group_by(word) %>%
  summarise(freq = n())

set.seed(1234) # for reproducibility 
wordcloud(words = words$word, freq = words$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=words, size=0.6, color='random-dark')
wordcloud2(data=words, size = 0.7, shape = 'pentagon')
