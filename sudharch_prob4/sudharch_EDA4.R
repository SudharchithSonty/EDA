library(devtools)
library(twitteR)
library(RJSONIO)
library(stringr)
library(RCurl)
library(ROAuth)
library(jsonlite)
#SET UP TWITTER AUTHORIZATION
api_key <- "AZDoQTHcFjatMKkN2bIO8zvIA"
api_secret <- "CH4Q5OeU52E32Afv5DVjZ3eTy1HK2SuMvrF6pFSEtvVYxW88oX"
access_token <- "95933057-yYG7ZsuGcTu4a3FKxPeDGq5prpRUXlsCZT8qtscxP"
access_token_secret <- "iBgkkSpmsr3mFMEwgZ80E5B2Gl9mGQ0b9XeenRYJeuYbj"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#ENTER SEARCH QUERY 
query <- "#RealDirect, #rentalsRealDirect, #salesRealDirect"
query <- unlist(strsplit(query,","))
tweets = list()
no.of.tweets <- 500
for(i in 1:length(query))
{
  result <- searchTwitter(query[i], n=no.of.tweets, lang="en", since="2015-02-25")
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}
file<-NULL
df <- do.call("rbind", lapply(tweets, as.data.frame))
df<-rbind(df,file)
#REMOVE DUPLICATES
df <- df[!duplicated(df[c("id")]),]

currentTime <- Sys.time() 
csvFileName <- paste("/Users/Admin/EDA1/RealDirect",currentTime,".csv",sep="") 
write.csv(df,file=csvFileName,row.names=FALSE)


username='@RealDirect'
#the most tweets we can bring back from a user timeline is the most recent 3600...
mht=userTimeline(username,n=3200)
df=twListToDF(mht)

#CODE TO GENERATE FILE NAMES
currentTime <- Sys.time() 
csvFileName <- paste("/Users/Admin/EDA1/tweets",currentTime,".csv",sep="") 
write.csv(df,file=csvFileName,row.names=FALSE)


library(ggplot2)
library(plyr)
library(doBy)
#read the csv data
alldata <- read.csv(url("https://www.quandl.com/api/v3/datasets/ZILL/C00001_A.csv"))
price_to_rent <- read.csv(url("https://www.quandl.com/api/v3/datasets/ZILL/C00001_PRR.csv"))
increasing_value <- read.csv(url("https://www.quandl.com/api/v3/datasets/ZILL/C00001_IV.csv"))
homes_to_rent <- read.csv(url("https://www.quandl.com/api/v3/datasets/ZILL/C00001_HR.csv"))

str(alldata)
str(increasing_value)

hist(x =alldata$Value)
hist(x =increasing_value$Value)
hist(x = price_to_rent$Value)
