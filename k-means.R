library(factoextra)
library(tidyverse)
library(twitteR)
library(tidytext)
source("authenticate.R")

# Input values
topic <- "trump"
number_of_tweets <- 1000
exclude_words <- c(topic, "https", "t.co", "rt", "amp","travelban")

authenticate()

fn_twitter <- searchTwitter(topic,n=number_of_tweets,lang="en")
fn_twitter_df <- twListToDF(fn_twitter)


tweet_words <- fn_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)


tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                      hjust = 1)) + xlab("")

# Create a list of stop words: a list of words that are not worth including

my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = exclude_words))
