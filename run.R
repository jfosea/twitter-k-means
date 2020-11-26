source("tweet-scraping.R")
source("data-transform.R")


# ======================= INPUT =========================

# [character] The topic word to search tweets by.
word <- "#trump"

# [integer] The maximum number of tweets to scrape. May scrape fewer if not enough tweets match the topic.
n <- 10

# [character] If not NULL, restricts tweets to those since the given date.
# Date is to be formatted as YYYY-MM-DD
since <- "2020-11-24"

# [character] If not NULL, restricts tweets to those up until the given date.
# Date is to be formatted as YYYY-MM-DD
until <- "2020-11-25"

# [boolean] If TRUE, will scrape tweets live from the twitter. If FALSE, will retrieve tweet data from tweets_raw.csv
live <- TRUE

# ====================== SCRAPING ========================
data <- scrape_tweets(word, n, since, until, FALSE)
tweets <- as.data.frame(data[1])
df <- as.data.frame(data[2])
common_words <- as.data.frame(data[3])
hashtags <- as.data.frame(data[4])
number_of_tweets <- as.integer(data[5])

# ======================= KMEANS =========================
# testing out different number of clusters
k2 <- kmeans(df,2,nstart=25)
k3 <- kmeans(df,3,nstart=25)
k4 <- kmeans(df,4,nstart=25)
k5 <- kmeans(df,5,nstart=25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)

# ======================= ANALYSIS =======================

# some basic clustering statistics
k3


# add cluster group number variable
tweets$cluster <- k3$cluster


# plot of 10 most common words
p5 <- ggplot(common_words, aes(x = reorder(word, n, function(n) -n), y=n)) +
  geom_bar(stat="identity", fill="lightblue")+ theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("") + ggtitle("Top 10 Most Common Words")
p5


# plot hashtags
p_hashtags <- ggplot(hashtags, aes(x = reorder(Hashtags, n, function(n) -n), y=n)) +
  geom_bar(stat="identity", fill="darkblue")+ theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("") + ggtitle("Hashtags")
p_hashtags


# compare scores in each cluster
p6 <- count(tweets, cluster, score) %>% ggplot( aes(fill=score, y=n, x=cluster)) +
  geom_bar(position="stack", stat="identity")+theme_minimal() +
  ggtitle("Score Distribution Grouped by Clusters")
p6

# compare retweet count in each cluster
p7 <- quantile_plot(tweets$retweetCount, tweets$cluster) + ggtitle("Retweet Count")
# compare follower count in each cluster
p7
p8 <- quantile_plot(tweets$followers, tweets$cluster) + ggtitle("Followers Count")
p8
# compare total tweets count in each cluster
p9 <- quantile_plot(tweets$total_tweets, tweets$cluster) + ggtitle("Total Tweets Count")
p9
# compare tweet length in each cluster
p10 <- quantile_plot(tweets$tweet_length, tweets$cluster) + ggtitle("Tweet Length")
p10


grid.arrange(p6, p_hashtags, p7, p8, p9, p10, nrow = 2)

# compare
top_5 <- top_n_tweets(tweets,k3,5)
top_5 %>% group_by(cluster) %>% select(screenName, followers, total_tweets, location, score)




