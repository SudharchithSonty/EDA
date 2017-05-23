library(doBy)
library(ggplot2)
# files <-
#   list.files(path = "/Users/Admin/EDA1/dds_ch2_nyt",pattern = "*.csv")
# 
# files = lapply(files, read.csv, header = F, stringsAsFactors = F)
# files = do.call(rbind,files)
# 
# 
# 
# getwd()
# setwd("/Users/Admin/EDA1/dds_ch2_nyt")

urlpart1="http://stat.columbia.edu/~rachel/datasets/nyt"
urlpart2=".csv"
files=read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv")) #put day1 data first
for(i in 2:31){
  wholeurl=paste(paste(urlpart1,i,sep=""),urlpart2,sep="") #construct the url for different days urlpart1+i+urlpart2
  subdataframe=read.csv(url(wholeurl))  
  files=rbind(files,subdataframe) 
 }

#Question 1: Categorizing people as pr the age group

head(files)
files$agecat <- cut(files$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

#view
summary(files)


siterange <- function(x) {
  c(length(x), min(x), mean(x), max(x))
}
summaryBy(Age ~ agecat, data = files, FUN = siterange)
#only signed-in users have ages and genders
summaryBy(Gender + Signed_In + Impressions + Clicks ~ agecat,data = files)
#make the plot
ggplot(files, aes(x = Impressions, fill = agecat)) + geom_histogram(binwidth = 1)
ggplot(files, aes(x = agecat, y = Impressions, fill = agecat)) + geom_boxplot()


#we are creating click through rate, clicks only matter if there are impressions

files$hasimps <- cut(files$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks ~ hasimps, data = files, FUN = siterange)
ctr <- files$Clicks / files$Impressions
files$ctr <- ctr
ggplot(subset(files, Impressions > 0), aes(x = ctr, colour = agecat)) + geom_density()
ggplot(subset(files, Clicks > 0), aes(x = ctr,colour = agecat)) + geom_density()
ggplot(subset(files, Clicks > 0), aes(x = agecat, y = Clicks,fill = agecat)) + geom_boxplot()
ggplot(subset(files, Clicks > 0), aes(x = Clicks, colour = agecat)) + geom_density()

#creating categories

files$scode[files$Impressions == 0] <- "No Imps"
files$scode[files$Impressions > 0] <- "Imps"
files$score[files$Clicks > 0] <- "Clicks"

#Converting columns to factor

files$scode <- factor(files$scode)
head(files)

#Looking at Levels

clen <- function(x) {
  c(length(x))
}
etable <-
  summaryBy(Impressions ~ scode + Gener + agecat, data = files, FUN = clen)
