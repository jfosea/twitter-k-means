#' Cleans tweets, converts categorical variables to numerical, and scales all columns
#'
#' @param topic string of topic to search for.
#' @param tweets data.frame that contains the raw tweets scraped using twitteR
#' @param number_of_tweets int number of tweets to scrape.
#' @return dataframe of cleaned and scaled tweets
process_tweets <- function(topic, tweets, number_of_tweets) {
  
  exclude <- data.frame(word = c(topic,
                                 paste0(topic, c("'s" , "'s", "s")),
                                 "https",
                                 "t.co",
                                 "rt",
                                 "amp",
                                 "it's",
                                 paste0(1:100)))
  
  
  # Create list of unwanted words
  my_stop_words <- stop_words %>% select(-lexicon) %>%
    bind_rows(exclude)
  
  # get user information
  followers <- c()
  user_created <- c()
  location <- c()
  total_tweets <- c()
  user_dictionary <- hash()
  for (i in 1:number_of_tweets) {
    user <- get_user(tweets$screenName[i], user_dictionary)
    followers <- append(followers, user$followersCount)
    location <- append(location, user$location)
    total_tweets <- append(total_tweets, user$statusesCount)
  }
  
  tweets$followers <- followers
  tweets$location <- location
  tweets$total_tweets <- total_tweets
  
  # Separate each tweet by word
  tweet_words <- tweets %>% select(id,text) %>% unnest_tokens(word,text)
  
  # Filter out stop words
  tweet_words_clean <- tweet_words %>% anti_join(my_stop_words)
  common_words <- tweet_words_clean %>% count(word, sort=TRUE) %>% head(10)
  
  # adding score column if tweet contains common words
  score <- rep(0, number_of_tweets)
  for (i in 1:nrow(common_words)) {
    ids <- tweet_words_clean %>% filter(word==common_words[i,1]) %>% select(id) %>% unique()
    for (j in 1:length(ids[,1])) {
      n <- which(tweets$id == ids[j,1])
      score[n] <- sum(score[n], 1)
    }
  }
  
  # select only the numerical variable
  tweets$score <- score
  tweets_num <- select(tweets, total_tweets, followers, retweetCount, score)
  
  # transform different categorical values into numerics
  tweets_num$isRetweet <- as.numeric(tweets$isRetweet)
  tweets_num$created <- as.numeric(tweets$created)
  tweets_num$location <- as.numeric(as.factor(tweets$location))
  
  
  # scale and return dataframe
  return(list(tweets, scale(tweets_num)))

}



top_5_tweets_indices <- function (x) {
  tweet_indices <- rep(0, length(x$centers[,1]))
  for (i in 1:length(x$centers[,1])) {
    ind <- which(x$cluster==i)
    distances <- rowSums(df[ind,] - x$centers[i,])^2
    distdata <- data.frame(ind, distances)
    a <- distdata %>% arrange(distances) %>% head(5)%>% select(ind) 
    tweet_indices[i] <- a
  }
  return(tweet_indices)
}



top_5_tweets <- function(data, x) {
  tweet_indices <- rep(0, length(x$centers[,1]))
  for (i in 1:length(x$centers[,1])) {
    ind <- which(x$cluster==i)
    distances <- rowSums(df[ind,] - x$centers[i,])^2
    distdata <- data.frame(ind, distances)
    a <- distdata %>% arrange(distances) %>% head(5)%>% select(ind) 
    tweet_indices[i] <- a
  }
  
  top_5 <- data.frame()
  for (i in 1:length(tweet_indices)) {
    top_5 <- rbind(top_5,data.frame(data[unlist(tweet_indices[i]),], cluster=i))
  }
  return(top_5)
}


#' Get the user object corresponding to the given user_name.
#' 
#' @param user_name string of the username to get the user object of.
#' @param user_dictionary has<string, user> diciontary of usernames and corresponding user objects for users already found. This exists to reduce the number of API calls to retrieve users.
#' @returns the user object corresponding to the given user_name.
get_user <- function(user_name, user_dictionary) {
  #return(getUser(user_name))
  
  if (!has.key(user_name, user_dictionary)) {
    user <- getUser(user_name)
    user_dictionary[user_name] <- user
  } else {
    user <- user_dictionary[user_name]
  }
  return(user)
}
