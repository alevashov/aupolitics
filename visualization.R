# visualization of the data
library(tidyverse)
library(rtweet)
library(ggthemes)
library(RMySQL)
library(ggrepel)
library(DT)
# setting DB connection and getting users and tweets
# note it may take a while to load tweets
source("dbconnectiondetails.R")
con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)
users <- dbReadTable(con, 'users2')
tweets <- dbReadTable(con, 'tweets_party')
dbDisconnect(con)

# filter last month

t <- tweets %>% filter(as.Date(created_at)>as.Date(today()-30)) %>% group_by(party)
# status updates by party
ts_plot(t, by = "3 days", trim=1, tz="Australia/Sydney") +
        ggplot2::theme_light() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
                x = NULL, y = NULL,
                title = "Frequency of AU MPs Twitter statuses last 30 days",
                subtitle = "Twitter status (tweet) counts aggregated using 3-day intervals",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
# text summary
u <- users %>% group_by (party) %>%
        summarise(followers=sum(followers_count), likes = sum(favourites_count), statuses=sum(statuses_count))
u
# get rid of small parties and combine nationals and liberals to coalition
u <- u %>% filter(party %in% c ("Australian Labor Party", "Liberal Party of Australia", "The Nationals")) 
u$party <- gsub("The Nationals", "Coalition", u$party)
u$party <- gsub("Liberal Party of Australia", "Coalition", u$party)
u$party <- gsub("Australian Labor Party", "ALP", u$party)
u1 <- group_by(u, party) %>% summarise_all(funs(sum))


library(reshape2)

u2 <- melt(u1, id="party")
bar <- group_by(u2, variable, party) %>% summarise(mean=mean(value))

d <-ggplot(bar, aes(x=variable, y=mean, fill=factor(party)))
d+geom_bar(position = "dodge", stat="identity")+
        labs(
                x = NULL, y = NULL,
                title = "Twitter metrics. Labor with Coalition",
                caption = "\nSource: Data collected from Twitter's REST API via rtweet",
                fill = "Party\n"
        )

# Twitter clients and devices
# overall
clients <- tweets %>% group_by(source=as.factor(source)) %>% summarise(count = n()) %>% 
        top_n(5, count) %>% arrange(desc(count))

clients %>% ggplot(aes(reorder(source,count),count, fill=source))+
        geom_col()

# by party as percentage
clients_party <- tweets %>% group_by(party=as.factor(party), source=as.factor(source)) %>% summarise(count = n()) %>% 
        arrange(party, desc(count)) %>% filter (party %in% c("Australian Labor Party", "Liberal Party of Australia",
        "The Nationals")) %>% top_n(5, count) %>% mutate(perc=count/sum(count))

clients_party %>% ggplot(aes(x=party, y=perc*100, fill=reorder(source,count), label = round(perc*100,0)))+
        geom_col()+
        labs(y="Percent", fill="Source", title = "Twitter sources/devices")+
        geom_text(size = 3, position = position_stack(vjust = 0.5))+
        theme_economist()
####
#top 100 tweets, excluding retweets in table. By sum of fav and retweets

top <- t %>% filter(party=="Australian Labor Party", is.na(retweet_status_id)) %>%
  select(screen_name, text, retweet_count, favorite_count, created_at, status_url, party) %>%
  arrange (desc(favorite_count))  %>% plain_tweets() %>% top_n(100, favorite_count+retweet_count)


# plots
topF <- t %>% filter(party=="Australian Labor Party", is.na(retweet_status_id)) %>%
  select(screen_name, text, retweet_count, favorite_count, created_at, status_url, party) %>%
  arrange (desc(favorite_count))  %>% plain_tweets() %>% top_n(20, favorite_count)

topF %>% ggplot(aes(x=mday(created_at), y=favorite_count, size=favorite_count, 
                   color=screen_name))+
  geom_point()

topR <- t %>% filter(party=="Australian Labor Party", is.na(retweet_status_id)) %>%
  select(screen_name, text, retweet_count, favorite_count, created_at, status_url, party) %>%
  arrange (desc(favorite_count))  %>% plain_tweets() %>% top_n(20, retweet_count)

topR %>% ggplot(aes(x=mday(created_at), y=retweet_count, size=retweet_count, 
                    color=screen_name))+
  geom_point()
### as table

top <- top %>% mutate (link=paste0('<a href=', status_url, ' target="_blank" >On Twitter</a>'))

top %>% ungroup() %>% select(text, screen_name, retweets=retweet_count, likes=favorite_count, link) %>%
                 datatable(escape = FALSE)

# all tweets over period in table
t %>% filter (is.na(retweet_status_id)) %>% plain_tweets() %>%
  mutate (link=paste0('<a href=', status_url, ' target="_blank" >On Twitter</a>')) %>%
  select (text, screen_name, retweets=retweet_count, likes=favorite_count, link, party) %>%
  top_n (1000, likes+retweets) %>% arrange(likes) %>% 
  datatable(escape = FALSE)
