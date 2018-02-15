# learning text analytics

install.packages("tidytext")
library(tidytext)

# chapter 3

rm(list=ls())
install.packages("janeaustenr")
library(janeaustenr)
library(tidyr)
library(dplyr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

austen_bigrams

austen_bigrams %>% count(bigram, sort= TRUE)

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

bigram_united

bigram_tf_idf <- bigram_united %>% 
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

library(ggplot2)

bigram_tf_idf %>% arrange(desc(tf_idf)) %>% 
  mutate(word=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(book) %>%
  top_n(15, tf_idf) %>% ungroup() %>%
  ggplot(aes(x=word, y=tf_idf, fill=book))+
    geom_col()+facet_wrap(~book, ncol=2, scales="free" )+
    coord_flip()