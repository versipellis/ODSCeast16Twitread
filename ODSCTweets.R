library(twitteR)
library(stringr)
library(qdap)
library(tm)
library(RWeka)
library(xlsx)

key1 <- ""
key2 <- ""
key3 <- ""
key4 <- ""
twitterHandle <- ""

options(stringsAsFactors = FALSE) #strings not factors
Sys.setlocale('LC_ALL','C') #helps with multi-language errors
setup_twitter_oauth(key1,key2,key3,key4)

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}
clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower)) #could use tryTolower or tolower
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  return(corpus)
}
tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

odsc <- searchTwitter('#ODSC',n=5000,since='2016-05-18',until='2016-05-25',retryOnRateLimit=500)

odscDF <- twListToDF(odsc)
odscDFnoRT <- twListToDF(strip_retweets(odsc))

custom.reader <- readTabular(mapping=list(content="text", id="id",screenName="screenName"))
corpus <- VCorpus(DataframeSource(odscDFnoRT), readerControl=list(reader=custom.reader))
corpus<-clean.corpus(corpus)

dtm<-DocumentTermMatrix(corpus)
tdm<-TermDocumentMatrix(corpus)
dtm.tweets.m<-as.matrix(dtm)
tdm.tweets.m<-as.matrix(tdm)

tdm.m <- as.matrix(tdm)
tdm.v <- sort(rowSums(tdm.m),decreasing=TRUE)
tdm.df <- data.frame(word = names(tdm.v),freq=tdm.v, row.names=NULL)

bigram_tdm <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizer))
bigramtdm.m <- as.matrix(bigram_tdm)
bigramtdm.v <- sort(rowSums(bigramtdm.m),decreasing=TRUE)
bigramtdm.df <- data.frame(word = names(bigramtdm.v),freq=bigramtdm.v)

#export and save
write.xlsx(odscDF, file = "twitterData.xlsx", sheetName = "AllTweets", row.names = FALSE)
write.xlsx(odscDFnoRT, file="twitterData.xlsx", sheetName="AllTweetsNoRT", row.names = FALSE, append = TRUE)
write.xlsx(tdm.df, file="twitterData.xlsx", sheetName="TDM", row.names = FALSE, append = TRUE)
write.xlsx(bigramtdm.df, file="twitterData.xlsx", sheetName="bigramTDM", row.names = FALSE, append = TRUE)

#Personal stats
self<-twListToDF(userTimeline('bennyjtang',n=200,includeRts=TRUE))
selfODSC<-subset(self,created>="2016-05-20")
selfODSC<-selfODSC[grep("#ODSC",selfODSC$text),]
sum(selfODSC$isRetweet) #Times I retweeted
sum(selfODSCnoRT$retweetCount) #Times retweeted
sum(selfODSCnoRT$favoriteCount) #Times favorited

