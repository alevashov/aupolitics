# Analysis part. Start from loading required packages
require(dplyr)
require(stringr)
require(tm)
require(wordcloud)
require(SnowballC)

# load data - tweets we collected

load("tweets.Rdata")

## get only text from list with tweets
gettext<- function (mylist) {
        text <- character()
        for (i in 1:length(mylist)){
        text <- append(text,  mylist[[i]][[".->text"]])        
        }
        return(text)        
}
# for some reasons unlist inside function doesn't work, so sample call should look like
text <- gettext(unlist(tweets_nat))

# build and clean corpus

corp <- function (text) {
        text_df <- data_frame(line = 1, text = text)     
        # clean up some stuff
        text_df <- sapply(text_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
        #corpus is a collection of text documents
        t_corpus <- Corpus(VectorSource(text_df))
        #clean text and stemming
        t_clean <- tm_map(t_corpus, removePunctuation)
        t_clean <- tm_map(t_clean, content_transformer(tolower))
        t_clean <- tm_map(t_clean, removeWords, stopwords("english"))
        t_clean <- tm_map(t_clean, removeNumbers)
        t_clean <- tm_map(t_clean, stripWhitespace)
        t_clean <- tm_index(t_clean, stemDocument)
        return(t_clean)        
}

# gettiin corpus for each party

lab_corp <- gettext(unlist(tweets_lab)) %>% corp()
lib_corp <- gettext(unlist(tweets_lib)) %>% corp()
nat_corp <- gettext(unlist(tweets_nat)) %>% corp()

#wordcloud function
mywordcloud <- function (corp) {
        wc <-wordcloud(corp, random.order=F,max.words=80, col=rainbow(80), scale=c(3,0.2))        
        return(wc)
}


# building wordclouds for parties
#setting parameters
layout(matrix(c(1, 2), nrow=2), heights=c(0.25, 4))
par(mar=rep(0, 4))
# Labor
plot.new()
text(x=0.5, y=0.5, "Labor")
lab_wc <- lab_corp %>% mywordcloud()
dev.copy(png,"labor.png",width=8,height=6,units="in",res=100)
dev.off()
#Libs
plot.new()
text(x=0.5, y=0.5, "Liberals")
lib_wc <- lib_corp %>% mywordcloud()
dev.copy(png,"liberals.png",width=8,height=6,units="in",res=100)
dev.off()
# Nationals
plot.new()
text(x=0.5, y=0.5, "Nationals")
nat_wc <- nat_corp %>% mywordcloud()
dev.copy(png,"nationals.png",width=8,height=6,units="in",res=100)
dev.off()
## Reducing dimensions - one document per party

# defining function
my_tm <- function (corp, name){
        term.matrix <- termFreq (corp) %>% as.matrix() 
        colnames(term.matrix)<- name
        # transpose to merge later
        #term.matrix <- t(term.matrix)
        term.matrix <- as.data.frame(term.matrix)
        term.matrix$word <- row.names(term.matrix)
        return(term.matrix)
}
# to new df
atm_nats <-  my_tm (nat_corp, "Nationals")
atm_libs <-  my_tm (lib_corp, "Liberals")
atm_labs <-  my_tm (lab_corp, "Labor")
# join
atm <- full_join(atm_nats, atm_libs, by = 'word') %>% full_join(atm_labs,by = 'word')
# formatting for further use
row.names(atm)<-atm$word
atm <- atm[c(1,3,4)]
atm <- as.matrix(atm)
atm[is.na(atm)] <- 0
# save in file 
write.csv(atm, 'word-party-freq-matrix.csv')
# compare 

# Comparison Cloud, words that are party specific
plot.new()
text(x=0.5, y=0.5, "Comparison Cloud")
comparison.cloud(atm,max.words=80,scale=c(3,.2), random.order=FALSE, colors=brewer.pal(max(3,ncol(atm)),"Dark2"),
                 use.r.layout=FALSE, title.size=3,
                 title.colors=NULL, match.colors=FALSE,
                 title.bg.colors="grey90")
dev.copy(png,"comparisoncloud.png",width=8,height=6,units="in",res=100)
dev.off()
# Commonality Cloud - similar words used by all parties
plot.new()
text(x=0.5, y=0.5, "Commonality Cloud")
commonality.cloud(atm, max.words=80,random.order=FALSE)       
dev.copy(png,"commonalitycloud.png",width=8,height=6,units="in",res=100)
dev.off()

