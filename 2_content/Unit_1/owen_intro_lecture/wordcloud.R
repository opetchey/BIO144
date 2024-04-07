library(tidyverse)
library(janitor)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tidytext)
library(readxl)

words <- read_xlsx("What words would you use to describe a
high quality data analysis_(1-1).xlsx") %>%
  clean_names() %>%
  rename(word = what_words_would_you_use_to_describe_a_high_quality_data_analysis) %>%
  group_by(word) %>%
  summarise(freq = n())

set.seed(1234) # for reproducibility 
wordcloud(words = words$word, freq = words$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=words, size=0.6, color='random-dark')
wordcloud2(data=words, size = 0.7, shape = 'pentagon')

words <- read_csv("2_content/Unit_1/owen_intro_lecture/What words would you use to describe a high quality data analysis_ (Responses).csv") %>%
  clean_names() %>%
  rename(word = see_above) %>%
  unnest_tokens(word, word) %>%
  anti_join(get_stopwords()) %>%
  group_by(word) %>%
  summarise(freq = n())

set.seed(1234) # for reproducibility 
wordcloud(words = words$word, freq = words$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=words, size=0.6, color='random-dark')
wordcloud2(data=words, size = 0.7, shape = 'pentagon')

