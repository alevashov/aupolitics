# aupolitics
Australian politics research, based on data from Twitter and other open sources

The workflows at the moment, listed below in the order required to run

## Tweets text analysis

1. data-collection.R - scrapping Australian parlament website for data of Members of Parlaments (MPs)
Reference to blog post with more detailed explanation of the approach - https://levashov.biz/scrapping-data-about-australian-politicians-with-rselenium/
2. tweets-collection.R - collecting tweets from MPs
3. analysis.R - analysis of data

## Data collection to the database on regular basis and futher visualization 

1. tweet-update.R script runnning daily on schedule, collects the data and saves to the database
2. visualization.R some visualization through R console
3. AUPolVizual/app.R Shiny R Application with visualization, available publically at https://rserv.levashov.biz/shiny/rstudio/

