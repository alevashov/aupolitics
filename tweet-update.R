# updates of tweets for users, to run daily
library(rtweet)
library(dplyr)
library(RMySQL)
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


#setting DB connection
source("dbconnectiondetails.R")
con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)
# reading users
users <- dbReadTable(con, 'users2')
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
total <- con %>% dbSendQuery("SELECT COUNT(status_id) FROM tweets2") %>% dbFetch()

# get user timelines and store in DB

for (i in 1:nrow (users)){
        t <- tsearch(users$screen_name[i], 25)
        t <- flatten(t)
        print(i)
        Sys.sleep(3)
        # skip empty results
        if (length(t)!=0){
                dbWriteTable(con, "tweets2", t, row.names=FALSE, append=TRUE)        
        }
}
# record number of added tweets and write to DB

total_new <- con %>% dbSendQuery("SELECT COUNT(status_id) FROM tweets2") %>% dbFetch()  
journal <- data.frame("timestamp"=Sys.time(), "new_entries"=as.numeric(total_new-total))
dbWriteTable(con, "journal", journal, row.names=FALSE, append=TRUE) 

# closing DB connection
dbDisconnect(con)

