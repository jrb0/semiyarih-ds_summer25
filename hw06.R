library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(tidyverse)
library(wordcloud)

data <- austen_books()

# use regex to detect chapters, add linenumber variable
data <- data %>%
  group_by(book) %>%
  mutate(
    chapter = cumsum(str_detect(text, regex("^chapter\\s+\\d+", ignore_case = TRUE))),
    linenumber = row_number()
  ) %>%
  ungroup()

# separate data set that filters for only book = emma 
emma_data <- austen_books() %>%
  filter(book == "Emma") %>%
  group_by(book) %>%
  mutate(
    chapter = cumsum(str_detect(text, regex("^chapter\\s+\\d+", ignore_case = TRUE))),
    linenumber = row_number()
  ) %>%
  ungroup() %>%
  unnest_tokens(word, text)

head(data, 20)

tokenized <- data %>%
  unnest_tokens(word, text)

head(tokenized, 20)

# filter sentiment lexicons using assignment criteria
nrc <- get_sentiments("nrc") %>% filter(sentiment == "joy")
afinn <- get_sentiments("afinn") %>% filter(value >= 1)
bing <- get_sentiments("bing") %>% filter(sentiment == "positive")

# function to count the frequency of each word in the lexicons
count_freq <- function(data, lexicon) {
  data %>%
    inner_join(lexicon, by = "word") %>%
    count(word, sort = TRUE) %>%
    filter(n > 50)
}

# run function on emma_data for each lexicon
emma_nrc <- count_freq(emma_data, nrc)
emma_afinn <- count_freq(emma_data, afinn)
emma_bing <- count_freq(emma_data, bing)

# function to plot frequency
emma_plot_bar <- function(df, title) {
  ggplot(df, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = title, x = "Word", y = "Frequency") +
    theme_minimal()
}

# run plot function for each lexicon
emma_plot_bar(emma_nrc, "Joy words - NRC - Emma")
emma_plot_bar(emma_afinn, "Positive words - AFINN - Emma")
emma_plot_bar(emma_bing, "Positive words - Bing - Emma")

# creat word clouds using wordcloud() function

wordcloud(words = emma_nrc$word, freq = emma_nrc$n,
          scale = c(4, 0.5), 
          max.words = 100)
wordcloud(words = emma_afinn$word, freq = emma_afinn$n,
          scale = c(4, 0.5), 
          max.words = 100)
wordcloud(words = emma_bing$word, freq = emma_bing$n,
          scale = c(4, 0.5), 
          max.words = 100)