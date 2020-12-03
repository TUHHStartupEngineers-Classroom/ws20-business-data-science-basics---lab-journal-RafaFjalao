library(httr)
library (rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library (keyring)
library(RSQLite)
alphavantage_api_url <- "https://www.alphavantage.co/query"
ticker               <- "WDI.DE"

keyring::key_set("token")
GET(alphavantage_api_url, query = list('function' = "GLOBAL_QUOTE",
                                       symbol     = ticker,
                                       apikey     = key_get("token")))



appname <- "myfirstdataanalysis"
key <- "S7toD7UeyzhvAh7vDFs1UlIJu"
secret <- "TZlLx5Ao4ZSGTMUrqvdBGulFO33XeUHDRYKbhUE6c6mCUHAV2G"

access_token <- "1325156787662696448-y9q7ebO1fX6050yxttGnZOqcQIWMRH"

access_secret <- "Xw0bLMOfqCtn5c9VxYJ1AkSDstd0Uawq0zHyjxPv1Fslx"

twitter_token <- create_token(app = appname,consumer_key = key,consumer_secret = secret,access_token = access_token,access_secret = access_secret)

#the first 5000 Tweets that used Election Fraud and mention @realDonaldTrump 

election_fraud_tweets <- search_tweets(q = "Election Fraud @realDonaldTrump", include_rts = FALSE, n = 5000)
# view the first 3 rows of the dataframe
head( election_fraud_tweets, n = 100)

selection <-election_fraud_tweets%>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(6)
# Plots the top 5 Locations   
 selection[-c(1),] %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "") 
  


                               