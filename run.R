source("libraries.R")
source("tweet-scraping.R")
source("data-transform.R")


# Load needed libraries and authenticate using Twitter tokens
load_libraries()
authenticate()


# Choose topic word
word <- "trump"
n <- 100
tweet_df <- scrape_tweets(word, n)
df <- process_tweets(word,tweet_df,n)


# K means classifier
k_means <- kmeans(df,3,nstart=25)
fviz_cluster(k_means, data = df)
