library(twitteR)
library(dplyr)
library(purrr)
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)

ver="0.1"

# set up twitter auth. 
# First time this is run, it opens an auth page with twitter
# and saves it in ~/.httr-oauth (add to .gitignore)
# Subsequent runs work transparently.
setup_twitter_oauth("kDH81zCgbhcd5OXEmUTx9Q7ep",
                   "oruOAaEZgTVIVUEtzqmMtvtcK4zUJP3iMvf2QOub3NWlVXOUHn")

# We can request only 3200 tweets at a time; it will return fewer
# depending on the API
#tweets <- userTimeline("drchriscole", n = 100)
country = '#fra'
tweets <- searchTwitter("#eurovision + #fra", n=3000)
tweets_df <- tbl_df(map_df(tweets, as.data.frame))

# get details of the songs
songs = read.delim(file='song_list.txt', head=FALSE, sep='\t', row.names = 1)
songs = songs[,-5:-6]
names(songs) <- c('country', 'performer', 'song', 'language')

# subset columns
tweets <- tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") 

# split words and remove tweet link
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% c(stop_words$word, '#eurovision', "rt"),
         str_detect(word, "[a-z]"))

# output sorted list most common words
tweet_words %>% count(word, sort=TRUE)

# get list of words and their related sentiments
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

# filter by source (unused)
sources <- tweet_words %>%
  mutate(total_words = n()) %>%
  distinct(id, source, total_words)

# collate count of sentiments based on tweeted words
sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment) 

# plot counts for only the "positive" and "negative" sentiments
ggplot(sentiment[6:7,], aes(x=sentiment,y=n, fill=sentiment)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme(legend.position="none") +
  ggtitle(country)
