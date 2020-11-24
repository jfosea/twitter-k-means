source("tweet-scraping.R")
source("data-transform.R")


# Choose topic word and create data frame used for algorithm
word <- "trump"
n <- 400
df_raw <- scrape_tweets(word, n)
processed_data <- process_tweets(word,df_raw,n)
tweets <- as.data.frame(processed_data[1])
df <- as.data.frame(processed_data[2])

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

# obtaining indices of top 5 tweets nearest to centroid
top_5_tweets(tweets, k2)
top_5_tweets(tweets, k3)
top_5_tweets(tweets, k4)
top_5_tweets(tweets, k5)
