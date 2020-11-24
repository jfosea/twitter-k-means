#' Scrapes tweets from Twitter
#'
#' @param topic string of topic to search for.
#' @param number_of_tweets int number of tweets to scrape.
#' @param live boolean value that if TRUE, live scrape of tweets is to be returned
#' or if FALSE, premade tweets are returned
#' @return dataframe of tweets
scrape_tweets <- function(topic, number_of_tweets, live=FALSE) {
  if (live==FALSE) {
    # read from saved .csv files
    tweets <- read.csv("datasets/tweets.csv")
    df <- read.csv("datasets/scaled_tweets.csv")
    common_words <- read.csv("datasets/common_words.csv")
    return(list(tweets, df, common_words))
  }
  if (live==TRUE) {
    authenticate()
    # scrape from Twitter
    fn_twitter <- searchTwitter(topic,n=number_of_tweets,lang="en")
    df_raw <- twListToDF(fn_twitter) 
    # Clean and prepare analytical dataset
    processed_data <- process_tweets(topic, df_raw, number_of_tweet)
    # Separate results
    tweets <- as.data.frame(processed_data[1])
    df <- as.data.frame(processed_data[2])
    common_words <- as.data.frame(processed_data[3])
    return(list(tweets, df, common_words))
  }
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

