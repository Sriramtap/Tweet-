install.packages("devtools")
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
ls("package:sentiment")
install.packages("ROAuth")
install.packages("twitteR")

# install_url("http://cran.r-project.org/src/contrib/Archive/Snowball/Snowball_0.0-11.tar.gz")
# require(snowball)
#########################################

##     Intall packages            ################

###############################################
library(RCurl)
library(ROAuth)
library(twitteR)
library(plyr)
library(ggplot2)
library(reshape2)
library(sentiment)
library(wordcloud)
library(RColorBrewer)



#library(RCurl)
#library(ROAuth)
#library(twitteR)
#####################33 Load lib    ###################

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


##################       cacert.pem is a bundle of CA certificates that
##################       you use to verify that the server is really the 
##################       correct site you're talking to  

#Set constant request URL
requestURL <- "https://api.twitter.com/oauth/request_token"

# Set constant access URL
accessURL <- "https://api.twitter.com/oauth/access_token"

# Set constant auth URL
authURL <- "https://api.twitter.com/oauth/authorize"


# Put the both Consumer Key and Consumer Secret key from Twitter App.
###### Ref:- https://apps.twitter.com/app/15329643/keys



consumerKey <- "zzmmM85GIzRucXI1tksbFNdDl"  
consumerSecret <- "fQ0IONMyodBcfHWzA6GkULiU5hZB1eg4utehLPOM63szY2vxa6"

#Create the authorization object by calling function OAuthFactory
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)


# Handshake Oauth; Get code and enter it on URL in Console
Cred$handshake(
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

############# Cred$handshake(
######   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
######   To enable the connection, please direct your web browser to: 
######  https://api.twitter.com/oauth/authorize?oauth_token=Rze3ngAAAAAA6elrAAABY9wQ3eE
############  When complete, record the PIN given to you and provide it here:

#OAUTH Authentication

consumerKey <- "zzmmM85GIzRucXI1tksbFNdDl" 
consumerSecret <- "fQ0IONMyodBcfHWzA6GkULiU5hZB1eg4utehLPOM63szY2vxa6"
access_Token <- "4065656414-K1ApxsV6qZGq8ZnQ8PYcQw1rANvzaYNEp4c13Q4" 
access_Secret <- "c0BAN5QruxivM1cUM9eGuwTEXtVJgSSB5rZA571woi2bY"


##### Reference for above key's https://apps.twitter.com/app/15329643/keys

# Create Twitter connection
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)


############# Enter the tweets to be collected 

tweets = searchTwitter('#NBAFinals', n=200, lang="en")
tweets

##############   Pre-processing the data

tweet.text = laply(tweets,function(t)t$getText())    

###########  For each element of a list, apply function then combine results into an array.

clean.text <- function(some_txt)
{  
  ### STEP 4.2(a): Remove punctualtion, links
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  ## STEP 4.2(b): Remove non-english characters
  some_txt = gsub("[^\x20-\x7E]", "", some_txt)
  
  # STEP 4.2(c) : Define "tolower error handling" function
  try.tolower = function(x)
  {  
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)

### Clean the content with "clean.text" function & remove qoute


tweet_txt = clean.text(tweet.text)
clean_text = clean.text(tweet_txt)

#############Transforming Text

### Build a corpus, which is a collection of text documents
#               VectorSource specifies that the source is character vectors


tweet_corpus = Corpus(VectorSource(clean_text))


tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c("machine", "learning", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))


#### TDM in matrix
m = as.matrix(tdm)

#######################################https://github.com/Mzkarim/SentimentAnalysis
##### decresing order of count
word_freqs = sort(rowSums(m), decreasing=TRUE) 

### Creating a data frame 
dm = data.frame(word=names(word_freqs), freq=word_freqs) 

#### Wordcloud

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) 
png("Cloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

sentiment <- data.frame(text = )
