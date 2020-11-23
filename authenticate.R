library(twitteR)

authenticate <- function() {
  consumer_key <- "8JUJeufLuUlqyqYsbxFBJp7aB"
  consumer_secret <- "l1tLcsUIU753GyDuttCDT3ACKguuPGD4Lnd7JRJXHEaEy5tJu1"
  access_token <- "377520293-EvuYxzVO5aZoTtXA0otIRfqxYG6HqCEDhjuJt4BK"
  access_secret <- "LbvA7nKJY0Wx4lR8QOt7gxb1pKRajA8gJi7MtXux0nQdh"
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

