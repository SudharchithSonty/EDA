library(doBy)
library(ggplot2)
# data1 <-
#   list.data1(path = "/Users/Admin/EDA1/dds_ch2_nyt",pattern = "*.csv")
# 
# data1 = lapply(data1, read.csv, header = F, stringsAsFactors = F)
# data1 = do.call(rbind,data1)
# 
# 
# 
# getwd()
# setwd("/Users/Admin/EDA1/dds_ch2_nyt")

#urlpart1="http://stat.columbia.edu/~rachel/datasets/nyt"
#urlpart2=".csv"
data1=read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv")) #put day1 data first
# for(i in 2:31){
#   wholeurl=paste(paste(urlpart1,i,sep=""),urlpart2,sep="") #construct the url for different days urlpart1+i+urlpart2
#   subdataframe=read.csv(url(wholeurl))
#   data1=rbind(data1,subdataframe) 
# }

#Question 1: Categorizing people as pr the age group

head(data1)
data1$agecat <- cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

#view
summary(data1)


siterange <- function(x) {
  c(length(x), min(x), mean(x), max(x))
}
summaryBy(Age ~ agecat, data = data1, FUN = siterange)
#only signed-in users have ages and genders
summaryBy(Gender + Signed_In + Impressions + Clicks ~ agecat,data = data1)
#make the plot
ggplot(data1, aes(x = Impressions, fill = agecat)) + geom_histogram(binwidth = 1)
ggplot(data1, aes(x = agecat, y = Impressions, fill = agecat)) + geom_boxplot()


#we are creating click through rate, clicks only matter if there are impressions

data1$hasimps <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks ~ hasimps, data = data1, FUN = siterange)
ctr <- data1$Clicks / data1$Impressions
data1$ctr <- ctr
ggplot(subset(data1, Impressions > 0), aes(x = ctr, colour = agecat)) + geom_density()
ggplot(subset(data1, Clicks > 0), aes(x = ctr,colour = agecat)) + geom_density()
ggplot(subset(data1, Clicks > 0), aes(x = agecat, y = Clicks,fill = agecat)) + geom_boxplot()
ggplot(subset(data1, Clicks > 0), aes(x = Clicks, colour = agecat)) + geom_density()

#creating categories

data1$scode[data1$Impressions == 0] <- "No Imps"
data1$scode[data1$Impressions > 0] <- "Imps"
data1$score[data1$Clicks > 0] <- "Clicks"

#Converting columns to factor

data1$scode <- factor(data1$scode)
head(data1)

#Looking at Levels

clen <- function(x) {
  c(length(x))
}
etable <-
  summaryBy(Impressions ~ scode + Gener + agecat, data = data1, FUN = clen)
