#' Scrapes tweets from Twitter
#'
#' @param topic character of topic to search for.
#' @param number_of_tweets int number of tweets to scrape.
#' @param common_word_count int The number of most common words to retrieve
#' @param since character if not NULL, restricts tweets to those since the given date. Date is to be formatted as YYYY-MM-DD
#' @param until character If not NULL, restricts tweets to those up until the given date. Date is to be formatted as YYYY-MM-DD
#' @param live boolean value that if TRUE, live scrape of tweets is to be returned
#' or if FALSE, premade tweets are returned
#' @return dataframe of tweets
scrape_tweets <- function(topic, number_of_tweets, common_word_count, since, until, live=FALSE) {
  if (live) {
    authenticate()
    # scrape from Twitter
    tweets_raw <- searchTwitter(topic,n=number_of_tweets, since=since, until=until, lang="en")
    df_raw <- tweets_raw %>% twListToDF() %>% select(-id)
    write.csv(df_raw, "datasets/tweets_raw.csv")
    
    # In case fewer tweets are returned than requested, update the
    # number_of_tweets for processing later on.
    number_of_tweets <- NROW(df_raw)

  } else {
    # read from saved .csv files
    df_raw <- read.csv("datasets/tweets_raw.csv", colClasses = c("created"="character")) %>% select(-X)
  }
  
  df_raw <- cbind(seq(1, NROW(df_raw)), df_raw)
  names(df_raw)[1] <- "id"

  # Clean and prepare analytical dataset
  processed_data <- process_tweets(topic, df_raw, number_of_tweets, common_word_count)
  # Separate results
  tweets <- as.data.frame(processed_data[1])
  df <- as.data.frame(processed_data[2])
  common_words <- as.data.frame(processed_data[3])
  hashtags <- as.data.frame(processed_data[4])

  return(list(tweets, df, common_words, hashtags, number_of_tweets))
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

