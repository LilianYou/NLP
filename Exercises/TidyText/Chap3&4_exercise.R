# plot tf_idf of bigrams in austen books 

library(dplyr)
library(janeaustenr)
library(tidytext)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_tf_idf <- austen_bigrams %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf2 <- bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "bigram_tf_idf", y = NULL)

bigram_tf_idf2

# find trigrams that occur more than once in the novel S&S
austen_books() %>% 
  filter(book == 'Sense & Sensibility') %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) %>% 
  ggplot(aes(n, word2, fill = n > 1)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

library(tidyr)
library(forcats)
SS <- austen_books() %>% 
  filter(book == 'Sense & Sensibility') %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  count(trigram, sort = TRUE) %>% 
  ggplot(aes(n, forcats::fct_reorder(trigram,n), fill = n > 1)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")
