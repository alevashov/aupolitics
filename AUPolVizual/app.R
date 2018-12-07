#
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
   
   # Application title
   titlePanel("Australian Member of Parliament Twitter statistics"),
   
   # Sidebar with a selector for party
   sidebarLayout(
      sidebarPanel(
         selectInput("party", "Party:",
                      c("Australian Labor Party", "Liberal Party of Australia", "The Nationals"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("devPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$devPlot <- renderPlot({
      # generate col chart for party based on input$party from ui.R
           clients <- t %>% filter(party==input$party) %>% group_by(source=as.factor(source)) %>% 
                   summarise(count = n()) %>% 
                   top_n(5, count) %>% arrange(desc(count)) 
           # draw the histogram with the specified number of bins
           clients %>% ggplot(aes(reorder(source,count),count, fill=source, label = count))+
                   geom_col()+
                   labs(x="", y="Number of tweets", fill="Source", title = "Twitter sources/devices")+
                   geom_text(size = 3, position = position_stack(vjust = 0.5))+
                   theme_dark()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

