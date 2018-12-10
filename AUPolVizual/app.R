# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(tidyverse)
library(rtweet)
library(ggthemes)
library(RMySQL)
library(lubridate)
# setting DB connection and getting users and tweets
setwd("~/aupolitics")
source("dbconnectiondetails.R")
# note it may take a while to load tweets
# implement conditional update logic, only update once per day

# define update as a function
updateDb <- function (){
con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)
t <- dbReadTable(con, 'tweets_party')
save(t, file="tweets_app.RData")
dbDisconnect(con)
}

# load tweets from local file and compare by status_id with Db
load("tweets_app.RData")
max_local <- as.numeric(max(t$status_id))
con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)
max_db <- con %>% dbSendQuery("SELECT MAX(status_id) FROM twitterr.tweets_party;") %>% dbFetch()
dbDisconnect(con)
max_db <- as.numeric(max_db[1])
if (max_local < max_db) updateDb()


# Define UI for application that draws a histogram
ui <- fluidPage(
   # Google Analytics code
   tags$head(includeHTML("ga.html")),
   # Application title
   titlePanel("Australian Members of Parliament Twitter statistics. Updated daily"),
   
   # Sidebar with a selector for party
   sidebarLayout(
      sidebarPanel(
         selectInput("party", "Party:",
                      c("Australian Labor Party", "Liberal Party of Australia", "The Nationals")),
         dateInput("start_date", "Start date:", value=today()-30, min=today()-90, max=today()-1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Devices",plotOutput("devPlot")),
          tabPanel("Timeline", plotOutput("timePlot")),
          tabPanel("Top tweets", DT::dataTableOutput("toptweetsTable"))
        )
      )
   ),
   # Footer notes
   hr(),
   div(class = "footer",
       includeHTML("footer.html")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$devPlot <- renderPlot({
      # generate col chart for devices used per party based on input$party from ui.R
           t1 <- t %>% filter(as.Date(created_at)>as.Date(input$start_date))
           clients <- t1 %>% filter(party==input$party) %>% group_by(source=as.factor(source)) %>% 
                   summarise(count = n()) %>% 
                   top_n(5, count) %>% arrange(desc(count)) 
           # draw the col chart
           clients %>% ggplot(aes(reorder(source,count),count, fill=source, label = count))+
                   geom_col()+
                   labs(x="", y="Number of tweets", fill="Source", title = "Twitter sources/devices")+
                   geom_text(size = 3, position = position_stack(vjust = 0.5))+
                   theme_economist()
           
      
   })
   output$timePlot <- renderPlot({
     # status updates by party since date selected
     t1 <- t %>% filter(as.Date(created_at)>as.Date(input$start_date))
     t_p <- t1 %>% filter(party==input$party)
     ts_plot(t_p, by = "1 day", trim=1, tz="Australia/Sydney") +
       ggplot2::theme_light() +
       ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
       ggplot2::labs(
         x = NULL, y = NULL,
         title = paste0("Frequency of selected party MPs tweets, since ", as.character(input$start_date)),
         subtitle = "Tweets counts aggregated using 1-day intervals, trimmed",
         caption = "\nSource: Data collected from Twitter's REST API via rtweet"
       )+
       theme_economist()
     
   })
   output$toptweetsTable <- DT::renderDataTable({
     # find top 200 tweets for the party over given period of time
      
       t1 <- t %>% filter(as.Date(created_at)>as.Date(input$start_date)) %>%
       filter(party==input$party) %>% ungroup() %>%
       filter (is.na(retweet_status_id)) %>% plain_tweets() %>%
       mutate (link=paste0('<a href=', status_url, ' target="_blank" >On Twitter</a>')) %>%
       select (text, screen_name, retweets=retweet_count, likes=favorite_count, link, party) %>%
       top_n (200, likes+retweets) %>% arrange(likes) 
       DT::datatable(t1, escape = FALSE, 
                     options = list(order = list(list(4, 'desc'), list(3, 'desc')))
                     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

