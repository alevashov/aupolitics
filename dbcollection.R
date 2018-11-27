# retired version, because TwitteR package not supported actively anymore
# new version is dbcollection-rtweet.R with rtweet package

# TwitteR with DB 
library(twitteR)
library(dplyr)
#import twitter accounts

mps <- read.csv('mps.csv')
#filter out NAs

mps <- mps %>% filter(twitter!="NA") %>% group_by(party)

#setting DB connection
source("dbconnectiondetails.R")
register_mysql_backend(db_name=db_name, host=host, user=user, password=pw)
# setting twitter authentication
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
source("api_key.r") #local file, not in global repo for security reasons
setup_twitter_oauth(apikeys[1], apikeys[2],apikeys[3], apikeys[4])

users <- lookupUsers (as.character(mps$twitter))                
store_users_db(users, table_name="users")

tsearch <- function (username, n=100){
        q=paste0('from:',as.character(username))
        #result <-searchTwitter(q, resultType="recent", n=n, retryOnRateLimit=5)
        result <- userTimeline(as.character(username), n=n)
        return(result)
}
for (i in 77:96){
        t <- tsearch(users[[i]][[".->screenName"]])
        print(i)
        Sys.sleep(5)
        store_tweets_db(t, table_name="tweets")
}

t <- tsearch(users[[49]][[".->screenName"]])
store_tweets_db(t, table_name="tweets")

