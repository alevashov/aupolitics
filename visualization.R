# visualization of the data
library(tidyverse)
library(rtweet)

# setting DB connection and getting users and tweets
# note it may take a while to load tweets
source("dbconnectiondetails.R")
con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)
users <- dbReadTable(con, 'users2')
tweets <- dbReadTable(con, 'tweets2')
dbDisconnect(con)


up <- select(users, one_of(c("user_id", "party")))
t <- left_join(tweets, up, by="user_id")

# filter last month
t <- t %>% filter(as.Date(created_at)>as.Date("2018-09-01")) %>% group_by(party)
# status updates by party
ts_plot(t, by = "3 days", trim=1, tz="Australia/Sydney") +
        ggplot2::theme_light() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
                x = NULL, y = NULL,
                title = "Frequency of AU MPs Twitter statuses since Sep 2018",
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
    
