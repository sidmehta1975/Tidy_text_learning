# learning text analytics

install.packages("tidytext")
library(tidytext)

# chaapter 1

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

library(dplyr)
text_df <- data_frame(line=1:4, text=text)
text_df

# unnest_tokens()

text_df %>% unnest_tokens(word, text)

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>% unnest_tokens(word, text)
tidy_books
summary(tidy_books)
tidy_books %>% filter(book == "Sense & Sensibility") %>% summary()

# remove stop words
tidy_books <- tidy_books %>% anti_join(stop_words)
tidy_books %>% count(word, sort=TRUE)

# word frequencies

install.packages("gutenbergr")
library(gutenbergr)


hgwells <- gutenberg_download (c(35, 36, 5230, 159), meta_fields = "title", 
                             mirror = "http://mirrors.xmission.com/gutenberg/") 

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767),
                             mirror = "http://mirrors.xmission.com/gutenberg/")

head(hgwells)
tail(hgwells)
summary(hgwells)
View(hgwells)

# pre processing 

tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>%  
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)
  
head(frequency)
summary(frequency)
View(frequency)
