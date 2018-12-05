#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rtweet)
library(ggthemes)
library(RMySQL)
# setting DB connection and getting users and tweets
# note it may take a while to load tweets
# source("dbconnectiondetails.R")
# con <- dbConnect(RMySQL::MySQL(), dbname= db_name, username=user, password=pw, host=host, port=3306)
# users <- dbReadTable(con, 'users2')
# tweets <- dbReadTable(con, 'tweets2')
# dbDisconnect(con)
up <- select(users, one_of(c("user_id", "party")))
t <- left_join(tweets, up, by="user_id")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Australian Member of Parliament Twitter stats"),
   
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
                   labs(y="Number of tweets", fill="Source", title = "Twitter sources/devices")+
                   geom_text(size = 3, position = position_stack(vjust = 0.5))+
                   theme_dark()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

