library(twitteR)

authenticate <- function() {
  consumer_key <- "insert here"
  consumer_secret <- "insert here"
  access_token <- "instert here"
  access_secret <- "insert here"
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

