#' Scrapes tweets from Twitter
#'
#' @param topic string of topic to search for.
#' @param number_of_tweets int number of tweets to scrape.
#' @return dataframe of tweets
scrape_tweets <- function(topic, number_of_tweets) {
  token <- get_authentication_token()

  fn_twitter <- rtweet::search_tweets(
    q = topic,
    n = number_of_tweets,
    token = token,
    retryonratelimit = TRUE)

  # twListToDF(fn_twitter)
}

#' Get authentication token for accessing Twitter API
#'
#' @returns string token to confirm credential authentication
get_authentication_token <- function() {
  credentials <- read.csv(file="credentials.csv")[1,]
  token <- create_token(
    app="CPSC571-F20-Problem3",
    consumer_key = credentials$consumer_key,
    consumer_secret = credentials$consumer_secret,
    access_token = credentials$access_token,
    access_secret = credentials$access_token)

  return(token)
}

