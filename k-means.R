library(factoextra)
library(tidyverse)
library(twitteR)
library(tidytext)

consumer_key <- "8JUJeufLuUlqyqYsbxFBJp7aB"
consumer_secret <- "l1tLcsUIU753GyDuttCDT3ACKguuPGD4Lnd7JRJXHEaEy5tJu1"
access_token <- "377520293-EvuYxzVO5aZoTtXA0otIRfqxYG6HqCEDhjuJt4BK"
access_secret <- "LbvA7nKJY0Wx4lR8QOt7gxb1pKRajA8gJi7MtXux0nQdh"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

fn_twitter <- searchTwitter("#Houston",n=1000,lang="en")
fn_twitter_df <- twListToDF(fn_twitter)
tweet_words <- fn_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                      hjust = 1)) + xlab("")