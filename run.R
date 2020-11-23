source("tweet-scraping.R")
source("data-transform.R")


# Choose topic word and create data frame used for algorithm
word <- "trump"
n <- 100
tweet_df <- scrape_tweets(word, n)
df <- process_tweets(word,tweet_df,n)


# K means classifier
k_means <- kmeans(df,3,nstart=25)
fviz_cluster(k_means, data = df)
