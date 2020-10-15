library(ndjson)
library(tidyverse)
library(tidytext)
library(widyr)
library(tm)

# convert json to data.frame/tibble and then reduce to only needed variables
tweets.data <- stream_in('quilting.json') %>%
  as_tibble() %>%
  select(created_at, text, user.description, user.favourites_count, user.followers_count, user.friends_count, user.location, user.name, user.screen_name, user.statuses_count, entities.user_mentions.0.screen_name, retweeted_status.user.screen_name, entities.urls.0.expanded_url, retweeted_status.user.entities.url.urls.0.expanded_url, retweet_count) %>%
  rename(tweet.datetime = created_at, total.favorites = user.favourites_count, total.followers = user.followers_count, total.friends = user.friends_count, screen.name = user.screen_name, total.statuses = user.statuses_count, tweet.mentions = entities.user_mentions.0.screen_name, retweet.mentions = retweeted_status.user.screen_name, url = entities.urls.0.expanded_url, retweet.url = retweeted_status.user.entities.url.urls.0.expanded_url, total.retweets = retweet_count)

# common artifacts that remain after cleaning
other.words <- c("rt", "amp","htt")

# remove all urls
tweets.data$text <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", tweets.data$text)

# clean data
tweets.data$text <- tweets.data$text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("SMART")) %>%
  removeWords(other.words) %>%
  stemDocument() %>%
  stripWhitespace()

# transform table into one-word-per-line tidytext format
tidy.data <- tweets.data %>%
  unnest_tokens(word, text)

# most frequent words
tidy.data %>%
  count(word, sort = TRUE)

# bigrams
bigram.data <- tweets.data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# explore bigrams 
bigram.data %>%
  count(bigram, sort = TRUE)

# pairwise word correlations
word.cors <- tidy.data %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, url, sort = TRUE)

# explore top word correlations
word.cors

# most frequent words
tidy.data %>%
  count(word, sort = TRUE)

# explore specific word correlations
# try a couple different words for fun
word.cors %>%
  filter(item1 == "etsi")

# produce graph comparing 4 words of importance to their most correlated words
word.cors %>%
  filter(item1 %in% c("quilt", "fabric", "sew", "etsi")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

############### sentiment ###################
bing.negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# explore
bing.negative

# scores word sentiment according to bing lexicon
bing.word.counts <- tidy.data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# visualizes to positive and negative words according to bing
bing.word.counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
