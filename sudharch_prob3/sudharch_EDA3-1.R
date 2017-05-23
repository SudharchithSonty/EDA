require(gdata)
require(dplyr)
require(plyr)
require(devtools)
require(ggplot2)
require(ggvis)
library(foreign)
bk <- read.csv("/Users/Admin/EDA1/dds_ch2_rollingsales/rollingsales_manhattan.csv")
#  bk1 <- read.csv("/Users/Admin/EDA1/dds_ch2_rollingsales/rollingsales_queens.csv")
# bk2 <- read.csv("/Users/Admin/EDA1/dds_ch2_rollingsales/rollingsales_bronx.csv")
#  bk3 <- read.csv("/Users/Admin/EDA1/dds_ch2_rollingsales/rollingsales_statenisland.csv")
#  mergeset1 <- merge(bk,bk1,all=TRUE)
#  mergeset2 <- merge(bk2,bk3, all=TRUE)
#  bk <- merge(mergeset1, mergeset2, all=TRUE)
#  head(bk)
#  summary(bk)







names(bk) <- tolower(names(bk))

## clean/format the data with regular expressions

bk$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",
                                   bk$sale.price))
count(is.na(bk$sale.price.n))
bk$grosssqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                 bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))
## do a bit of exploration to make sure there's not anything ## weird going on with sale prices
attach(bk) 
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(grosssqft[sale.price.n==0])
detach(bk)
## keep only the actual sales 
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$grosssqft,bk.sale$sale.price.n)
plot(log(bk.sale$grosssqft),log(bk.sale$sale.price.n))
## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",
                                bk.sale$building.class.category)),]
plot(log(bk.homes$grosssqft),log(bk.homes$sale.price.n))
bk.homes[which(bk.homes$sale.price.n<100000),
         order(bk.homes[which(bk.homes$sale.price.n<100000),]
               $sale.price.n),]
## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$grosssqft),log(bk.homes$sale.price.n))