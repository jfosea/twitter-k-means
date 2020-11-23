library(factoextra)
library(tidyverse)
library(twitteR)
library(tidytext)
source("authenticate.R")


# Input values
topic <- "trump"
number_of_tweets <- 100
exclude <- data.frame(word = c(topic,
                               paste0(topic, c("'s" , "’s", "s")),
                               "https",
                               "t.co",
                               "rt",
                               "amp",
                               "it’s",
                               paste0(1:100)))


# Create list of unwanted words
my_stop_words <- stop_words %>% select(-lexicon) %>%
  bind_rows(exclude)


authenticate()

# Search for tweets related to the topic word
tweets_raw <- searchTwitter(topic,n=number_of_tweets,lang="en")
tweets <- twListToDF(tweets_raw)

#======================== DATA PREPROCESSING ========================

# Get user information of each tweet
followers <- c()
user_created <- c()
location <- c()
total_tweets <- c()
for (i in 1:number_of_tweets) {
  user <- getUser(tweets$screenName[i])
  followers <- append(followers, user$followersCount)
  user_created <- append(user_created, as.Date(user$created))
  location <- append(location, user$location)
  total_tweets <- append(total_tweets, user$statusesCount)
}

tweets$followers <- followers
tweets$user_created <- user_created
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
tweets_num$num_created <- as.numeric(tweets$created)
tweets_num$num_user_create <- as.numeric(tweets$user_created)




# rename rows to username and scale the data and rename the rows
row.names(tweets_num) <- make.names(tweets$score, unique=TRUE)
df <- scale(tweets_num)


#======================== K MEANS  ========================
# K means classifier
k_means <- kmeans(df,3,nstart=25)
fviz_cluster(k_means, data = df)
