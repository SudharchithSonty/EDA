library(twitteR)
library(stringr)
library(RCurl)
library(ROAuth)
library(jsonlite)
library(streamR)
#SET UP TWITTER AUTHORIZATION
api_key <- "AZDoQTHcFjatMKkN2bIO8zvIA"
api_secret <- "CH4Q5OeU52E32Afv5DVjZ3eTy1HK2SuMvrF6pFSEtvVYxW88oX"
access_token <- "95933057-yYG7ZsuGcTu4a3FKxPeDGq5prpRUXlsCZT8qtscxP"
access_token_secret <-
  "iBgkkSpmsr3mFMEwgZ80E5B2Gl9mGQ0b9XeenRYJeuYbj"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#ENTER SEARCH QUERY
query <- "Donald Trump,Hilary Clinton,US Elections,Bernie Sanders"
query <- unlist(strsplit(query,","))
tweets = list()
no.of.tweets <- 500
for (i in 1:length(query))
{
  result <-
    searchTwitter(query[i], n = no.of.tweets, lang = "en", since = "2016-02-27")
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}
file <- NULL
df <- do.call("rbind", lapply(tweets, as.data.frame))
df <- rbind(df,file)
#REMOVE DUPLICATES
df <- df[!duplicated(df[c("id")]),]
#CODE TO GENERATE FILE NAMES
currentTime <- Sys.time()
jsonFileName <-
  paste("/Users/Admin/EDA1/tweets",currentTime,".json",sep = "")


myjson.df <- toJSON(df)

#CONVERT TO JSON
write(myjson.df,file = jsonFileName)

document <- fromJSON(file = "/Users/Admin/EDA1/Part1/tweets4.json")
View(document)