#' Scrapes tweets from Twitter
#'
#' @param topic string of topic to search for.
#' @param number_of_tweets int number of tweets to scrape.
#' @return dataframe of tweets
scrape_tweets <- function(topic, number_of_tweets) {
  authenticate()

  fn_twitter <- searchTwitter(topic,n=number_of_tweets,lang="en")
  twListToDF(fn_twitter)
}

#' Connects to Twitter API
authenticate <- function() {
  credentials <- read.csv(file="credentials.csv")[1,]
  consumer_key <- credentials[1]
  consumer_secret <- credentials[2]
  access_token <- credentials[3]
  access_secret <- credentials[4]

  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

