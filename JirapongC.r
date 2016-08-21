#source('JirapongC.r')

#assignment 1: export stock data
#create function to export stock data
getStockData <- function(URL,stockName)
{
library(XML)
wd <- getwd()
tmpFullPath <- paste(wd,'/tmp',sep='')
allTables <- readHTMLTable(URL)
write.table(allTables[6],tmpFullPath,sep="|",quote = F)
stock <- read.table(tmpFullPath, sep = "|", skip=2 )
stockPrice <- data.frame(stock)
stockPrice <- stockPrice[,c("V2","V6","V7")]
cName<-c("date",paste(stockName,'_price',sep=''),paste(stockName,'_volume',sep=''))
names(stockPrice) <- cName
stockPrice <- lapply(stockPrice, function(x) {gsub(",", "", x)})
return(stockPrice)
}
aapl <- getStockData('http://www.nasdaq.com/symbol/aapl/historical','aapl')
goog <- getStockData('http://www.nasdaq.com/symbol/goog/historical','goog')
#merge aapl and goog into stockPrice data frame
stockPrice <- merge(aapl,goog,by="date")
#sort data frame by date desc
stockPrice <- data.frame(stockPrice)
stockPrice <- stockPrice[order(as.Date(stockPrice$date, format="%m/%d/%Y"),decreasing = T),]
#set output full path
wd <- getwd()
outputFullPath <- paste(wd,'/stockPrice.csv',sep='')
#export aapl and goog data
write.table(stockPrice, outputFullPath, sep=",",quote = F,row.names = F)

#assignment 2: export news data
#create function to export news data
getNewsData <- function(URL,newsType)
{
library(XML)
library(RCurl)
library(bitops)
parsedXML <- xmlParse(URL)
title <- xpathSApply(parsedXML,'//item/title',xmlValue)
description <- xpathSApply(parsedXML,'//item/description',xmlValue)
pubDate <- xpathSApply(parsedXML,'//item/pubDate',xmlValue)
News <- data.frame(title,description,pubDate)
News["news_type"] <- newsType
return(News)
}
stock <- getNewsData('http://articlefeeds.nasdaq.com/nasdaq/categories?category=Stocks','Stock')
mf <- getNewsData('http://articlefeeds.nasdaq.com/nasdaq/categories?category=Mutual+Funds','Mutual Fund')
#union stock and mf news
allNews <- rbind(mf, stock)
#re-format pubDate
allNews$pubDate <- lapply(allNews$pubDate, function(x) {gsub(" -0400", "", x)})
allNews$pubDate <- as.POSIXct(strptime(allNews$pubDate,format="%a, %d %b %Y %H:%M:%S"))
#sort data frame by pubDate desc
allNews <- allNews[order(allNews$pubDate,decreasing = T),]
#set output full path
outputFullPath <- paste(wd,'/News.csv',sep='')
#export news data
write.table(allNews, outputFullPath, sep=",",quote = T,row.names = F)

