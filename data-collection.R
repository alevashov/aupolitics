### scapping data about Australian Members of Parlament, from official website
install.packages("RSelenium")
library(RSelenium)

# datarframe for results
mps <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(mps)<- c('name', 'electorate', 'party', 'twitter')

## initalizing selenium driver

rD <- rsDriver()
remDr <- remoteDriver(port = 4567L, browserName = "chrome")
remDr$open()

### function to get most of the data about one MP
onempdata <- function (webElems, ref) {
        one <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(one)<- c('name', 'electorate', 'party', 'twitter')
        name <- webElems[[ref]]$findChildElement(using='class', value='title')
        one$name <- as.character(name$getElementText())
        electorate <- webElems[[ref]]$findChildElement(using='css selector', value='dd:nth-child(2)')
        one$electorate <- as.character(electorate$getElementText())
        ## party data is badly structured, will get it another way
        
        ### getting twitter, since not all MPs have it, we need to catch errors to avoid stop
        
        twitter <- tryCatch({
                suppressMessages({
                        webElems[[ref]]$findChildElement(using = 'css selector', value = '.fa-twitter')
                         
                })
        }, 
        error = function(e) {
                NA_character_
        }
        )
        # only collect Twitter handle if it exists      
        if (class(twitter)!='character'){
                twitter$clickElement()
                Sys.sleep(4)
                windows <- remDr$getWindowHandles() #get list of open windows 
                remDr$switchToWindow(windows[[2]]) # switch to Twitter window 
                tt <- as.character(remDr$getCurrentUrl())# collect URL
                remDr$closeWindow() # Close Twitter window 
                remDr$switchToWindow(windows[[1]]) #switch back to main window
                one$twitter <- as.character(tt)
        }        
        else one$twitter <- NA_character_ #if no Twitter return NA
        # return one row dataframe with all data about one MP
        return(one)
}





collectPartyData <- function (url, party){
        remDr$navigate(url)
        # empty dataframe to collect one party data
        fmps <- data.frame(matrix(ncol = 4, nrow = 0))
        colnames(fmps)<- c('name', 'electorate', 'party', 'twitter')
        webElems1 <- remDr$findElements(using = 'xpath', value = '//*[contains(concat( " ", @class, " " ), concat( " ", "padding-top", " " ))]')
        for (i in seq_along(webElems1)){
                one <- data.frame(matrix(ncol = 4, nrow = 1))
                colnames(one)<- c('name', 'electorate', 'party', 'twitter')
                one <- onempdata(webElems1, i)
                one$party <- party
                fmps <- rbind(fmps, one)
        }
        return (fmps)
}
## run with Labour MPS

url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=1&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "Australian Labor Party"))        

## run with Liberal MPS
url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=2&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "Liberal Party of Australia"))        
  
## run with Nationals
url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=3&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "The Nationals"))        

## run with Greens
url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=9&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "Australian Greens"))        
 
## run with independent
url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=4&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "Independent"))    

## run with Centre Alliance

url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=25&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "Centre Alliance"))    

### run with Katter party

url <- "https://www.aph.gov.au/Senators_and_Members/Parliamentarian_Search_Results?q=&mem=1&par=15&gen=0&ps=96"
mps <- rbind(mps,collectPartyData(url, "Katter's Australian Party"))    

### closing webdriver session
remDr$close()

### quick formatting twitter to leave only handle

mps$twitter <- gsub("https://twitter.com/","", mps$twitter)

write.csv(mps, "mps.csv")

