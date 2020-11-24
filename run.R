source("tweet-scraping.R")
source("data-transform.R")


# Choose topic word and create data frame used for algorithm
word <- "trump"
n <- 400
cluster_count <- 5
df_raw <- scrape_tweets(word, n)
processed_data <- process_tweets(word,df_raw,n)
tweets <- as.data.frame(processed_data[1])
df <- as.data.frame(processed_data[2])

# testing out different number of clusters
kmeans_results <- vector(mode="list", length = cluster_count)

for (i in 2:cluster_count) {
  assign(paste("kmeans_result", i, sep = ""), kmeans(df, i, nstart=25))
}

kmeans_plots <- vector(mode="list", length = cluster_count)

# plots to compare
for (i in 2:cluster_count) {
  kr <- get(paste("kmeans_result", i, sep=""))
  # assign(paste("kmeans_plot", i, sep=""), fviz_cluster(kr, geom = "point", data = df) + ggtitle(paste("k = ", i)))
  kmeans_plots[i] <- kr
}


grid.arrange(kmeans_plot2, kmeans_plot3, kmeans_plot4, kmeans_plot5, nrow = 2)

# obtaining indices of top 5 tweets nearest to centroid
for (i in 2:cluster_count) {
  top_5_tweets(tweets, kmeans_results[i])
}
