source("tweet-scraping.R")
source("data-transform.R")


# ======================= INPUT =========================
word <- "biden"
n <- 100
data <- scrape_tweets(word, n, live=FALSE)
tweets <- as.data.frame(data[1])
df <- as.data.frame(data[2])
common_words <- as.data.frame(data[3])

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

# ======================= ANALYSIS =========================

# some basic clustering statistics
k3


# add cluster group number variable
tweets$cluster <- k3$cluster


# plot of 10 most common words
p5 <- ggplot(common_words, aes(x = reorder(word, n, function(n) -n), y=n)) + 
  geom_bar(stat="identity", fill="lightblue")+ theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("") + ggtitle("Top 10 Most Common Words")
p5

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
grid.arrange(p7, p8, p9, nrow = 2)

# compare 
top_5 <- top_n_tweets(tweets,k3,5)
top_5 %>% group_by(cluster) %>% select(screenName, followers, total_tweets, location, score)




