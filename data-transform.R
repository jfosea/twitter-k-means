#' Cleans tweets, converts categorical variables to numerical, and scales all columns
#'
#' @param topic string of topic to search for.
#' @param tweets data.frame that contains the raw tweets scraped using twitteR
#' @param number_of_tweets int number of tweets to scrape.
#' @param common_word_count int The number of most common words to retrieve
#' @return dataframe of cleaned and scaled tweets
process_tweets <- function(topic, tweets, number_of_tweets, common_word_count) {

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
  total_tweets <- c()
  location <- c()
  tweet_length <- rep(0, number_of_tweets)
  for (i in 1:number_of_tweets) {
    user <- getUser(tweets$screenName[i])
    print(paste("user returned?", ifelse(is.null(user), "NO", "YES")))
    print(paste("user:", tweets$screenName[i]))
    print(paste("\tfollowers:", user$followersCount))
    followers <- append(followers, user$followersCount)
    location <- append(location, user$location)
    total_tweets <- append(total_tweets, user$statusesCount)
    tweet_length[i] <- nchar(tweets$text[i])
  }

  tweets$followers <- followers
  tweets$location <- location
  tweets$total_tweets <- total_tweets

  # add tweet_length as a new column to analyze
  tweets$tweet_length <- tweet_length

  # Separate each tweet by word
  tweet_words <- tweets %>% select(id,text) %>% unnest_tokens(word,text)

  # Filter out stop words
  tweet_words_clean <- tweet_words %>% anti_join(my_stop_words)
  common_words <- tweet_words_clean %>% count(word, sort=TRUE) %>% head(common_word_count)


  # Count top 10 most frequent hashtags
  hashtags <- list()
  for (i in 1:nrow(tweets)) {
    hashtags <-  append(hashtags, str_extract_all(tolower(tweets$text[i]), "#\\S+"))
  }

  hashtags_count <- count(data.frame(Hashtags=unlist(hashtags)),Hashtags, sort=TRUE) %>% head(10)

  # adding score column if tweet contains common words
  score <- rep(0, number_of_tweets)
  for (i in 1:nrow(common_words)) {
    ids <- tweet_words_clean %>% filter(word==common_words[i,1]) %>% select(id) %>% unique()
    for (j in 1:length(ids[,1])) {
      n <- which(tweets$id == ids[j,1])
      score[n] <- sum(score[n], 1)
    }
  }

  tweets$score <- score

  # create a copy of the tweets with numerical values only in order to be used for k-means clustering.
  # Select features that are relevant for analysis; discard the rest.
  tweets_num <- select(tweets, total_tweets, followers, retweetCount, favoriteCount, tweet_length, score)

  # transform different categorical values into numerics
  tweets_num$isRetweet <- as.numeric(tweets$isRetweet)
  tweets_num$created <- as.numeric(tweets$created)

  return(list(tweets, scale(tweets_num), common_words, hashtags_count))

}

#' Obtains n tweets in cluster closest to its centroid
#'
#' @param data dataframe that contains all tweet characteristics
#' @param x kmeans object of desired length of clusters
#' @param n int number of tweets to obtain from each cluster
#' @return dataframe of the top n closest tweets to centroids
top_n_tweets <- function(data, x, n) {
  tweet_indices <- rep(0, length(x$centers[,1]))
  for (i in 1:length(x$centers[,1])) {
    ind <- which(x$cluster==i)
    distances <- rowSums(df[ind,] - x$centers[i,])^2
    distdata <- data.frame(ind, distances)
    a <- distdata %>% arrange(distances) %>% head(n)%>% select(ind)
    tweet_indices[i] <- a
  }

  top_n <- data.frame()
  for (i in 1:length(tweet_indices)) {
    top_n <- rbind(top_n,data.frame(data[unlist(tweet_indices[i]),], cluster=i))
  }
  return(top_n)
}

#' Transforms selected continuous variable into categorical and plots barchart
#' stratified according to clusters
#'
#' @param col1 counting column of desired continuous variable to convert
#' @param col2 clustering column
#' @return ggplot object of barchart
quantile_plot <- function(col1, col2) {
  count_col <- col1
  cluster <- col2
  data <- data.frame(count_col, cluster)
  xs <- unique(ceiling(quantile(count_col,c(1/4,1/2,3/4,1))))

  print(paste("quantile length:", length(xs)))

  if (length(xs)==1) {
    xs <- unique(ceiling(quantile(count_col,c(1/2,1))))
    label1 <- xs

    data <- data %>% mutate(name=cut(count_col,
                                     breaks=c(-1,xs,Inf), labels=c(label1, "")))
    p <- count(data, cluster, name) %>% ggplot( aes(fill=name, y=n, x=cluster)) +
      geom_bar(position="stack", stat="identity")+theme_minimal()
  } else if (length(xs)==2) {
    xs <- unique(ceiling(quantile(count_col,c(1/2,1))))
    label1 <- ifelse(xs[1] == 0, 0, paste("0 -", xs[1]))
    label2 <- ifelse(xs[1] + 1 == xs[2], xs[2], paste(xs[1] + 1,"-", xs[2]))

    data <- data %>% mutate(name=cut(count_col,
                                     breaks=c(-1,xs,Inf), labels=c(label1, label2, "")))
    p <- count(data, cluster, name) %>% ggplot( aes(fill=name, y=n, x=cluster)) +
      geom_bar(position="stack", stat="identity")+theme_minimal()
  } else if (length(xs)==3) {
    xs <- unique(ceiling(quantile(count_col,c(1/3,2/3,1))))
    label1 <- ifelse(xs[1] == 0, 0, paste("0 -", xs[1]))
    label2 <- ifelse(xs[1] + 1 == xs[2], xs[2], paste(xs[1] + 1,"-", xs[2]))
    label3 <- ifelse(xs[2] + 1 == xs[3], xs[4], paste(xs[2] + 1,"-", xs[3]))

    data <- data %>% mutate(name=cut(count_col,
                                     breaks=c(-1,xs,Inf), labels=c(label1, label2, label3, "")))
    p <- count(data, cluster, name) %>% ggplot( aes(fill=name, y=n, x=cluster)) +
      geom_bar(position="stack", stat="identity")+theme_minimal()
  } else {
    label1 <- ifelse(xs[1] == 0, 0, paste("0 -", xs[1]))
    label2 <- ifelse(xs[1] + 1 == xs[2], xs[2], paste(xs[1] + 1,"-", xs[2]))
    label3 <- ifelse(xs[2] + 1 == xs[3], xs[4], paste(xs[2] + 1,"-", xs[3]))
    label4 <- ifelse(xs[3] + 1 == xs[4], xs[4], paste(xs[3] + 1,"-", xs[4]))

    data <- data %>% mutate(name=cut(count_col,
                                     breaks=c(-1,xs,Inf), labels=c(label1, label2, label3, label4, "")))
    p <- count(data, cluster, name) %>% ggplot( aes(fill=name, y=n, x=cluster)) +
      geom_bar(position="stack", stat="identity")+theme_minimal()
  }

  print(paste("quantile breaks:", xs))

  return(p)
}