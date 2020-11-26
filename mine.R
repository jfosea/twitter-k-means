credentials <- read.csv(file="credentials.csv")[1,]
consumer_key <- credentials[1]
consumer_secret <- credentials[2]
access_token <- credentials[3]
access_secret <- credentials[4]
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

topic <- "trump"
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
View(tweets)
tweet_words <- tweets %>% select(id,text) %>% unnest_tokens(word,text)
tweet_words_clean <- tweet_words %>% anti_join(my_stop_words)
common_words <- tweet_words_clean %>% count(word, sort=TRUE) %>% head(10)


hashtags <- list()
for (i in 1:10) {
  hashtags <-  append(hashtags, str_extract_all(tolower(tweets$text[i]), "#\\S+"))
}


hashtags_count <- count(data.frame(Hashtags=unlist(hashtags)),Hashtags, sort=TRUE) %>% head(10)
View(hashtags_count)
