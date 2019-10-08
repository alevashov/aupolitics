# updates of tweets for users, to run daily
# new version saving data to AWS S3 bucket instead of Database
library(rtweet)
library(dplyr)
library(aws.s3)
# define search function, update search for new tweets /status updates
# set n to 25 as reasonable number of tweets expected per day
tsearch <- function (username, n=25){
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

# set function to format tweet search results be compatible with stored data
tformat <- function (tw_data){
        tw_fixed <- tw_data %>% mutate (created_at = as.character(created_at), 
                            is_quote = as.integer(is_quote),
                            is_retweet = as.integer(is_retweet),
                            symbols = as.integer(symbols),
                            urls_url = as.character(urls_url),
                            urls_expanded_url = as.character(urls_expanded_url),
                            quoted_created_at = as.character(quoted_created_at),
                            quoted_verified = as.integer(quoted_verified),
                            retweet_created_at = as.character(retweet_created_at),
                            retweet_verified = as.integer(retweet_verified),
                            protected = as.integer(protected),
                            account_created_at = as.character(account_created_at),
                            verified = as.integer(0),
                            urls_t.co = as.character(urls_t.co),
                            account_lang= as.character(account_lang))
                                
        return(tw_fixed)
}

# set function to merge nicely tweets found with stored data, updating stored data if it existed
t_join <- function (t_old, t_new){
        t_new_dup <- t_new %>% filter(t_new$status_id %in% t_old$status_id)
        t_new_new <- t_new %>% filter(!(t_new$status_id %in% t_old$status_id))
        t_old[match(t_new_dup$status_id, t_old$status_id), ] <- t_new_dup
        t_old <- rbind(t_old, t_new_new)
        return(t_old)
}


#setting S3  connection, credentials are in env. variale
setwd("~/aupolitics")
source("aws-s3-conn.R")
# reading users
s3load("users.RData", bucket = "auspolrappdata")

# get simplified users dataframe for merge with new tweets later

u1 <- users %>% select(user_id, party)

# reading already stored tweets
s3load("tweets_app.RData", bucket = "auspolrappdata")


# checking and creating if needed twitter auth token

token <- tryCatch(get_token(), error= function(e){
        source("api_key.r") #local file with API details, not in global repo for security reasons
        # authorization in twitter
        
        create_token(
                app = appname,
                consumer_key = apikeys[1],
                consumer_secret = apikeys[2],
                access_token = apikeys[3],
                access_secret = apikeys[4])
        saveRDS(t_token, "~/.rtweet-oauth.rds")
        # ideally put this in ~/.Renviron
        Sys.setenv(TWITTER_PAT=path.expand("~/.rtweet-oauth.rds"))
        return(t_token)
        })

# record number of existing tweets for journal
total <- nrow(t)

# get user timelines and update dataframe


for (i in 1:nrow (users)){
        t1 <- tsearch(users$screen_name[i], 25)
        if (nrow(t1)<1) next
        t1 <- rtweet::flatten(t1)
        t1 <- left_join(t1, u1, by="user_id")
        t1 <- tformat(t1)
        print(i)
        Sys.sleep(3)
        # skip empty results
        if (length(t1)!=0){
                t <- t_join(t, t1)
        }
}
# record number of added tweets and write files to s3 bucket

# loading journal 

s3load("journal.RData", bucket = "auspolrappdata")

total_new <- nrow(t) - total
journal <- data.frame("timestamp"=Sys.time(), "new_entries"=as.numeric(total_new))
j <- rbind(j, journal)
## save to AWS
s3save(t, bucket = "auspolrappdata", object = "tweets_app.RData")
s3save(j, bucket = "auspolrappdata", object = "journal.RData") 

# save local copy

save(t, file="tweets_app.RData")





