# download jane austin book and create a character frequency figure

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

original_books

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(characters, text, token = 'characters')

tidy_books %>%
  count(characters, sort = TRUE) 

library(ggplot2)

tidy_books %>%
  count(characters, sort = TRUE) %>%
  mutate(characters = reorder(characters, n)) %>%
  ggplot(aes(n, characters)) +
  geom_col() +
  labs(y = NULL)

## show that if Jane Austen's book follow Zipf's law
frequency <- bind_rows(tidy_books) %>% 
  count(characters, sort = TRUE) %>%
  mutate(rank = row_number()) %>% 
  mutate(log_freq = log(n/sum(n)), log_freq2 = n/sum(n), log_rank = log(rank)) %>% 
  arrange(n)


# library(scales)
ggplot(frequency, aes(x = log_rank, y = log_freq)) + geom_point(aes(color = log_freq), size = 2)

# what is the mean sentiment per chapter of the book sense & sensitivity

get_sentiments("afinn")

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]",# div means looks for number or a greek letter
                                      ignore_case = TRUE)))) %>%  ungroup()


SS <- tidy_books %>% filter(book == 'Sense & Sensibility') %>% 
  unnest_tokens(word, text)

afinn <- SS %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = chapter) %>% 
  summarise(sentiment = mean(value)) %>% 
  ungroup()

ggplot(afinn, aes(index, sentiment)) +
  geom_line()
