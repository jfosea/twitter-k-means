library(factoextra)
library(tidyverse)
library(twitteR)
library(tidytext)
source("tweet-scraping.R")

#=======================Input values===============================
# Topic to search tweets by.
topic <- "trump"
# Number of tweets to analyze.
number_of_tweets <- 1000
# Words to exclude from keyword analysis. "Common" english words are already excluded.
exclude_words <- c(topic, "https", "t.co", "rt", "amp")
#==================================================================


# Scrap tweets live from Twitter.
df_tweets <- scrape_tweets(topic, number_of_tweets)

tweet_words <- df_tweets %>% select(id, text) %>% unnest_tokens(word,text)


tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                      hjust = 1)) + xlab("")

# Create a list of stop words: a list of words that are not worth including

my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = exclude_words))
