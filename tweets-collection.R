library(dplyr)
library(purrr)
mps <- read.csv('mps.csv')

# filter out MPS without twitters and smaller parties
majors <- c("Australian Labor Party", "Liberal Party of Australia", "The Nationals")
mp2 <- mps %>% filter(twitter!="NA" & party %in% majors) %>% group_by(party)

# check number of accounts
mp2 %>% summarise(n = n())

## a side task  - save all twitter handles to in one CSV file
tonly <- t(as.character(mp2$twitter))
write.csv(tonly, "th-only.csv", col.names = FALSE, row.names = FALSE, sep = ",")

library(twitteR)
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")

# You need your own API key in format
# apikeys <- c ('YOU API KEY', # api key
#               'YOUR API SECRET', # api secret
#               'YOU API ACCESS TOKEN', # access token
#               'YOUR API SECRET TOKEN') # access token secret)
# 
source("api_key.r") #local file, not in global repo for security reasons
setup_twitter_oauth(apikeys[1], apikeys[2],apikeys[3], apikeys[4])

labor <- mp2 %>% filter(party=="Australian Labor Party")
libs <- mp2 %>% filter(party=="Liberal Party of Australia")
nationals <- mp2 %>% filter(party=="The Nationals")

tsearch <- function (username, n=100){
        q=paste0('from:',as.character(username))
        result <-searchTwitter(q, resultType="recent", n=n, retryOnRateLimit=5)
        return(result)
}
# collecting tweets 
tweets_nat <- map(nationals$twitter, ~tsearch(.x, n=200))
tweets_lab<- map(labor$twitter, ~tsearch(.x, n=200))
tweets_lib<- map(libs$twitter, ~tsearch(.x, n=200))

## we need the last 3 objetcs in analysis script
save(tweets_nat, tweets_lab, tweets_lib, file = "tweets.RData")
