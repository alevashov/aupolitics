#  with RTweet and separate DB package
library(rtweet)
library(dplyr)
library(RMySQL)
#import twitter accounts

mps <- read.csv('mps.csv')
#filter out NAs

mps <- mps %>% filter(twitter!="NA")  
# remove trailing slash and changing to lower case for consistency
mps$twitter <- gsub("/", "", mps$twitter)
mps$twitter <- tolower(mps$twitter)
write.csv(mps, 'mps.csv')

source("api_key.r") #local file with API details, not in global repo for security reasons
# authorization in twitter

twitter_token <- create_token(
        app = appname,
        consumer_key = apikeys[1],
        consumer_secret = apikeys[2],
        access_token = apikeys[3],
        access_secret = apikeys[4]) 

saveRDS(twitter_token, "~/.rtweet-oauth.rds")
# ideally put this in ~/.Renviron
Sys.setenv(TWITTER_PAT=path.expand("~/.rtweet-oauth.rds"))

# getting users and formatting for merge
users <- lookup_users (mps$twitter)                
users <- flatten(users)
users$screen_name <- tolower(users$screen_name)
users <- users %>% left_join(mps, by = c("screen_name" = "twitter")) %>% group_by(party)


# define search function, initial search
tsearch <- function (username, n=200){
        result <- tryCatch({
                suppressMessages({
                        result <- get_timeline(username, n)
                        return(result)
                })
        }, 
        error = function(e) {
                NA_character_
        })
}


#setting DB connection
source("dbconnectiondetails.R")
con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)

# get user timelines and store in DB


for (i in 1:nrow (users)){
        t <- tsearch(users$screen_name[i], 200)
        t <- flatten(t)
        print(i)
        Sys.sleep(5)
        dbWriteTable(con, "tweets2", t, row.names=FALSE, append=TRUE)
}

# save users in DB
dbWriteTable(con, "users2", users, row.names=FALSE, append=TRUE)
# closing DB connection
dbDisconnect(con)

