library(tidyverse)
library(widyr)

newsgroup_cors <- tf_idf %>%
  pairwise_cor(newsgroup, word, tf_idf, sort = TRUE)

library(ggraph)
library(igraph)

newsgroup_cors %>%
  filter(correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Exercise 2
library(topicmodels)
library(ggplot2)
library(tidytext)

rel_lda %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 12) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## Exercise 2c
rel_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma, fill = factor(topic))) +
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  #coord_polar() +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")


## Exercise 3
negate_words <- c("extremely", "really", "totally", "very")

words_by_newsgroup %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Words preceded by a negation")

top_sentiment_words <- words_by_newsgroup %>%
  filter(str_detect(newsgroup, "^sci")) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))



