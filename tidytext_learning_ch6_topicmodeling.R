# learning text analytics

install.packages("tidytext")
library(tidytext)

# chapter 6

rm(list=ls())
install.packages("topicmodels")
library(topicmodels)
library(tidyr)
library(dplyr)
library(ggplot2)

data("AssociatedPress")
AssociatedPress

ap_lda <- LDA(AssociatedPress, k=2, control=list(seed= 1234))

# word topic probabilities

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

beta_spread_top10 <- beta_spread %>% top_n(10, log_ratio)
beta_spread_bottom10 <- beta_spread %>% top_n(10, -log_ratio)
beta_spread_plot <- bind_rows(beta_spread_top10, beta_spread_bottom10)

beta_spread_plot <- beta_spread_plot %>%
  mutate(term=factor(term, levels=rev(unique(term)))) %>%
  arrange(desc(log_ratio))

beta_spread_plot %>% ggplot(aes(reorder(term, log_ratio),log_ratio))+geom_col()+coord_flip()

# document topic probabilities

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents %>% arrange(document)

# the great library heist -----------------------------------------------------------------------

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title",
                     mirror = "http://mirrors.xmission.com/gutenberg/")

library(stringr)

# divide into documents, each representing one chapter 
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE))))%>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# LDA on chapters

# cast one-token-per-row table into a DocumentTermMaatrix for LDA modeling

chapters_dtm <- word_counts %>% 
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda


# per-topic-per-word
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

#top 10 terms per topic

library(ggplot2)
library(dplyr)

top_terms <- chapter_topics %>% group_by(topic) %>%
  top_n(10,beta) %>% ungroup() %>% arrange(topic, -beta)


top_terms %>% mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic)))+geom_col()+
  facet_wrap(~topic, ncol=2, scales="free")+  coord_flip()


# per-documeent-classification
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)


